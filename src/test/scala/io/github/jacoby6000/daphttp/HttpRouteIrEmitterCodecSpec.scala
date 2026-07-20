package io.github.jacoby6000.daphttp

import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.BitVector
import software.amazon.smithy.model.shapes.ShapeId

class HttpRouteIrEmitterCodecSpec extends AnyFunSuite {
  private def id(value: String): ShapeId = ShapeId.from(s"example#$value")

  private def compileSingleRead(
      memberTarget: IrType,
      memberPrimitiveOverride: Option[IrPrimitive] = None,
      endian: IrEndian = IrEndian.Big,
      wordSize: Option[Int] = Some(32),
      readSizeBytes: Option[Int] = None
  ): ReadPlan = {
    val output = IrType.EnclosingStruct(
      id = id("Output"),
      members = List(
        IrMember(
          id = id("Output_value"),
          name = "value",
          target = memberTarget,
          staticAddress = Some(0x1000L),
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = memberPrimitiveOverride,
          readSizeBytes = readSizeBytes
        )
      ),
      declaredSizeBits = None
    )

    HttpRouteIrEmitter
      .emitRoutePlansFromIr(
        List(
          IrService(
            name = "Api",
            wordSizeBits = wordSize,
            defaultEndian = endian,
            operations =
              List(IrOperation(name = "GetValue", routePath = "/api/Api/GetValue", output = output))
          )
        )
      )
      .routes("/api/Api/GetValue")
      .reads
      .head
  }

  private def decode(read: ReadPlan, bytes: Array[Byte]): Json =
    read.decodeCodec.get.decode(BitVector(bytes)).toOption.get.value

  test("decodes primitive codecs compiled from IR") {
    val cases = List(
      (
        IrPrimitive.U8,
        Some(32),
        IrEndian.Big,
        Array(0x7f.toByte),
        Json.fromLong(127L)
      ),
      (
        IrPrimitive.S8,
        Some(32),
        IrEndian.Big,
        Array(0xff.toByte),
        Json.fromLong(-1L)
      ),
      (
        IrPrimitive.U16,
        Some(32),
        IrEndian.Little,
        Array(0x34.toByte, 0x12.toByte),
        Json.fromLong(0x1234L)
      ),
      (
        IrPrimitive.S16,
        Some(32),
        IrEndian.Big,
        Array(0xff.toByte, 0xfe.toByte),
        Json.fromLong(-2L)
      ),
      (
        IrPrimitive.U32,
        Some(32),
        IrEndian.Big,
        Array(0x00.toByte, 0x00.toByte, 0x00.toByte, 0x2a.toByte),
        Json.fromLong(42L)
      ),
      (
        IrPrimitive.S32,
        Some(32),
        IrEndian.Big,
        Array(0xff.toByte, 0xff.toByte, 0xff.toByte, 0xd6.toByte),
        Json.fromLong(-42L)
      ),
      (
        IrPrimitive.U64,
        Some(32),
        IrEndian.Big,
        Array.fill[Byte](8)(0xff.toByte),
        Json.fromString("18446744073709551615")
      ),
      (
        IrPrimitive.S64,
        Some(32),
        IrEndian.Big,
        Array.fill[Byte](8)(0xff.toByte),
        Json.fromLong(-1L)
      ),
      (
        IrPrimitive.F8,
        Some(32),
        IrEndian.Big,
        Array(0x00.toByte),
        Json.fromDoubleOrNull(0.0)
      ),
      (
        IrPrimitive.F16,
        Some(32),
        IrEndian.Big,
        Array(0x00.toByte, 0x00.toByte),
        Json.fromDoubleOrNull(0.0)
      ),
      (
        IrPrimitive.F32,
        Some(32),
        IrEndian.Big,
        Array(0x3f.toByte, 0x80.toByte, 0x00.toByte, 0x00.toByte),
        Json.fromFloatOrNull(1.0f)
      ),
      (
        IrPrimitive.F64,
        Some(32),
        IrEndian.Big,
        Array(
          0x3f.toByte,
          0xf0.toByte,
          0x00.toByte,
          0x00.toByte,
          0x00.toByte,
          0x00.toByte,
          0x00.toByte,
          0x00.toByte
        ),
        Json.fromDoubleOrNull(1.0d)
      ),
      (
        IrPrimitive.Char,
        Some(32),
        IrEndian.Big,
        Array('A'.toByte),
        Json.fromString("A")
      ),
      (
        IrPrimitive.Bool,
        Some(32),
        IrEndian.Big,
        Array(0x80.toByte),
        Json.fromBoolean(true)
      ),
      (
        IrPrimitive.LongWord,
        Some(32),
        IrEndian.Big,
        Array(0x00.toByte, 0x00.toByte, 0x00.toByte, 0x2a.toByte),
        Json.fromLong(42L)
      )
    )

    cases.foreach { case (kind, wordSize, endian, bytes, expected) =>
      val read = compileSingleRead(IrType.Primitive(kind), endian = endian, wordSize = wordSize)
      withClue(s"primitive=$kind") {
        assert(decode(read, bytes) == expected)
      }
    }
  }

  test("decodes bitmask members into object fields") {
    val bitmask = IrType.Bitmask(
      id = id("Flags"),
      members = List(
        IrMember(
          id = id("Flags_ready"),
          name = "ready",
          target = IrType.Primitive(IrPrimitive.Bool),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = None
        ),
        IrMember(
          id = id("Flags_error"),
          name = "error",
          target = IrType.Primitive(IrPrimitive.Bool),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = None
        )
      ),
      declaredSizeBits = Some(2)
    )

    val read = compileSingleRead(bitmask)
    assert(decode(read, Array(0xc0.toByte)) == Json.obj("ready" -> Json.True, "error" -> Json.True))
  }

  test("decodes enclosing struct members when nested inside dap struct") {
    val nested = IrType.EnclosingStruct(
      id = id("Nested"),
      members = List(
        IrMember(
          id = id("Nested_a"),
          name = "a",
          target = IrType.Primitive(IrPrimitive.U8),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = None
        ),
        IrMember(
          id = id("Nested_b"),
          name = "b",
          target = IrType.Primitive(IrPrimitive.U8),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = None
        )
      ),
      declaredSizeBits = None
    )

    val dapStruct = IrType.MemoryMappedStruct(
      id = id("WrappedNested"),
      members = List(
        IrMember(
          id = id("WrappedNested_nested"),
          name = "nested",
          target = nested,
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = None
        )
      ),
      declaredSizeBits = Some(2)
    )

    val read = compileSingleRead(dapStruct)
    assert(
      decode(read, Array(0x01.toByte, 0x02.toByte)) ==
        Json.obj("nested" -> Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2)))
    )
  }

  test("decodes dap struct into json object with corresponding fields") {
    val dapStruct = IrType.MemoryMappedStruct(
      id = id("Registers"),
      members = List(
        IrMember(
          id = id("Registers_lo"),
          name = "lo",
          target = IrType.Primitive(IrPrimitive.U16),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = Some(IrPrimitive.U16)
        ),
        IrMember(
          id = id("Registers_hi"),
          name = "hi",
          target = IrType.Primitive(IrPrimitive.U16),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = Some(IrPrimitive.U16)
        )
      ),
      declaredSizeBits = Some(4)
    )

    val read = compileSingleRead(dapStruct)
    assert(
      decode(read, Array(0x12.toByte, 0x34.toByte, 0xab.toByte, 0xcd.toByte)) ==
        Json.obj("lo" -> Json.fromLong(0x1234L), "hi" -> Json.fromLong(0xabcdL))
    )
  }

  test("decodes char arrays as null-terminated strings") {
    val member = IrMember(
      id = id("Name_name"),
      name = "name",
      target = IrType.ListType(
        id = id("NameArray"),
        element = IrType.Primitive(IrPrimitive.Char),
        bytesAlias = false,
        bitsAlias = false
      ),
      staticAddress = None,
      paddingRepeats = None,
      isPointer = false,
      isArray = true,
      arrayLength = Some(8),
      endianOverride = None,
      primitiveOverride = Some(IrPrimitive.Char)
    )
    val struct = IrType.MemoryMappedStruct(
      id = id("Name"),
      members = List(member),
      declaredSizeBits = Some(8)
    )

    val read = compileSingleRead(struct)
    val bytes = Array[Byte](
      'H'.toByte,
      'e'.toByte,
      'l'.toByte,
      'l'.toByte,
      'o'.toByte,
      0,
      'x'.toByte,
      'x'.toByte
    )
    assert(decode(read, bytes) == Json.obj("name" -> Json.fromString("Hello")))
  }

  test("decodes char globals with symbol read width as null-terminated strings") {
    val read = compileSingleRead(
      memberTarget = IrType.Primitive(IrPrimitive.Char),
      memberPrimitiveOverride = Some(IrPrimitive.Char),
      readSizeBytes = Some(17)
    )
    val bytes = Array[Byte](
      'p'.toByte,
      'L'.toByte,
      'o'.toByte,
      'a'.toByte,
      'd'.toByte,
      'C'.toByte,
      'o'.toByte,
      'm'.toByte,
      'm'.toByte,
      'o'.toByte,
      'n'.toByte,
      'D'.toByte,
      'a'.toByte,
      't'.toByte,
      'a'.toByte,
      0x00.toByte,
      0x00.toByte
    )

    assert(decode(read, bytes) == Json.fromString("pLoadCommonData"))
  }

  test("decodes int enums to enumerator names and hex for unknown values") {
    val color = IrType.IntEnum(
      id = id("Color"),
      values = List(
        IrEnumValue("RED", 0),
        IrEnumValue("GREEN", 1),
        IrEnumValue("BLUE", 5)
      )
    )
    val read = compileSingleRead(color, endian = IrEndian.Big)

    assert(decode(read, Array(0x00, 0x00, 0x00, 0x00)) == Json.fromString("RED"))
    assert(decode(read, Array(0x00, 0x00, 0x00, 0x01)) == Json.fromString("GREEN"))
    assert(decode(read, Array(0x00, 0x00, 0x00, 0x05)) == Json.fromString("BLUE"))
    assert(decode(read, Array(0x00, 0x00, 0x00, 0x2a)) == Json.fromString("0x2a"))
  }

  test("decodes enum members inside dap structs") {
    val status = IrType.IntEnum(
      id = id("Status"),
      values = List(IrEnumValue("OK", 1), IrEnumValue("ERR", 2))
    )
    val dapStruct = IrType.MemoryMappedStruct(
      id = id("Packet"),
      members = List(
        IrMember(
          id = id("Packet_status"),
          name = "status",
          target = status,
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = None
        )
      ),
      declaredSizeBits = Some(4)
    )
    val read = compileSingleRead(dapStruct)

    assert(
      decode(read, Array(0x00, 0x00, 0x00, 0x01)) == Json.obj("status" -> Json.fromString("OK"))
    )
    assert(
      decode(read, Array(0x00, 0x00, 0x00, 0xff.toByte)) == Json.obj(
        "status" -> Json.fromString("0xff")
      )
    )
  }

  test("decodes duplicate enum values to the first enumerator name") {
    val aliases = IrType.IntEnum(
      id = id("Alias"),
      values = List(
        IrEnumValue("PRIMARY", 1),
        IrEnumValue("ALIAS", 1),
        IrEnumValue("OTHER", 2)
      )
    )
    val read = compileSingleRead(aliases)
    assert(decode(read, Array(0x00, 0x00, 0x00, 0x01)) == Json.fromString("PRIMARY"))
    assert(decode(read, Array(0x00, 0x00, 0x00, 0x02)) == Json.fromString("OTHER"))
  }

  test("decodes negative enum values from two's complement memory") {
    val status = IrType.IntEnum(
      id = id("SignedStatus"),
      values = List(IrEnumValue("OK", 0), IrEnumValue("NEG", -1))
    )
    val read = compileSingleRead(status)
    assert(
      decode(read, Array(0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte)) ==
        Json.fromString("NEG")
    )
  }

  test("decodes little-endian enum values") {
    val color = IrType.IntEnum(
      id = id("LeColor"),
      values = List(IrEnumValue("RED", 0), IrEnumValue("GREEN", 1))
    )
    val read = compileSingleRead(color, endian = IrEndian.Little)
    assert(decode(read, Array(0x01, 0x00, 0x00, 0x00)) == Json.fromString("GREEN"))
  }

  test("decodes arrays of enums to arrays of names") {
    val color = IrType.IntEnum(
      id = id("PaletteColor"),
      values = List(IrEnumValue("RED", 0), IrEnumValue("GREEN", 1), IrEnumValue("BLUE", 2))
    )
    val output = IrType.EnclosingStruct(
      id = id("Output"),
      members = List(
        IrMember(
          id = id("Output_colors"),
          name = "colors",
          target = IrType.ListType(
            id = id("ColorArray"),
            element = color,
            bytesAlias = false,
            bitsAlias = false
          ),
          staticAddress = Some(0x1000L),
          paddingRepeats = None,
          isPointer = false,
          isArray = true,
          arrayLength = Some(3),
          endianOverride = None,
          primitiveOverride = None
        )
      ),
      declaredSizeBits = None
    )
    val read = HttpRouteIrEmitter
      .emitRoutePlansFromIr(
        List(
          IrService(
            name = "Api",
            wordSizeBits = Some(32),
            defaultEndian = IrEndian.Big,
            operations =
              List(IrOperation(name = "GetValue", routePath = "/api/Api/GetValue", output = output))
          )
        )
      )
      .routes("/api/Api/GetValue")
      .reads
      .head

    val bytes = Array[Byte](
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x2a
    )
    assert(
      decode(read, bytes) == Json.arr(
        Json.fromString("RED"),
        Json.fromString("GREEN"),
        Json.fromString("0x2a")
      )
    )
  }

  test("annotateDecodedAddresses adds _address to root and nested structs") {
    val vec3 = IrType.MemoryMappedStruct(
      id = id("Vec3"),
      members = List(
        IrMember(
          id = id("Vec3_x"),
          name = "x",
          target = IrType.Primitive(IrPrimitive.F32),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = Some(IrPrimitive.F32),
          offsetBytes = Some(0)
        ),
        IrMember(
          id = id("Vec3_y"),
          name = "y",
          target = IrType.Primitive(IrPrimitive.F32),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = Some(IrPrimitive.F32),
          offsetBytes = Some(4)
        )
      ),
      declaredSizeBits = Some(8)
    )
    val outer = IrType.MemoryMappedStruct(
      id = id("Outer"),
      members = List(
        IrMember(
          id = id("Outer_id"),
          name = "id",
          target = IrType.Primitive(IrPrimitive.U32),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = Some(IrPrimitive.U32),
          offsetBytes = Some(0)
        ),
        IrMember(
          id = id("Outer_pos"),
          name = "pos",
          target = vec3,
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = None,
          offsetBytes = Some(0x10)
        ),
        IrMember(
          id = id("Outer_ptr"),
          name = "ptr",
          target = vec3,
          staticAddress = None,
          paddingRepeats = None,
          isPointer = true,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = None,
          offsetBytes = Some(0x20)
        )
      ),
      declaredSizeBits = Some(0x28 * 8)
    )

    val decoded = Json.obj(
      "id" -> Json.fromLong(1L),
      "pos" -> Json.obj("x" -> Json.fromDoubleOrNull(1.0), "y" -> Json.fromDoubleOrNull(2.0)),
      "ptr" -> Json.fromLong(0x80001000L)
    )

    val annotated =
      HttpRouteIrEmitter.annotateDecodedAddresses(outer, decoded, 0x80400000L, Some(32))

    assert(annotated.hcursor.downField("_address").as[String].toOption.contains("0x80400000"))
    assert(
      annotated.hcursor
        .downField("pos")
        .downField("_address")
        .as[String]
        .toOption
        .contains("0x80400010")
    )
    assert(annotated.hcursor.downField("pos").downField("x").as[Double].toOption.contains(1.0))
    // Pointer slots stay numeric — no struct wrapper / _address on the pointee value.
    assert(annotated.hcursor.downField("ptr").as[Long].toOption.contains(0x80001000L))
    assert(annotated.hcursor.downField("ptr").downField("_address").failed)
  }

  test("annotateDecodedAddresses annotates each struct array element") {
    val item = IrType.MemoryMappedStruct(
      id = id("Item"),
      members = List(
        IrMember(
          id = id("Item_v"),
          name = "v",
          target = IrType.Primitive(IrPrimitive.U32),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = Some(IrPrimitive.U32),
          offsetBytes = Some(0)
        )
      ),
      declaredSizeBits = Some(4)
    )
    val listType =
      IrType.ListType(id = id("ItemList"), element = item, bytesAlias = false, bitsAlias = false)
    val outer = IrType.MemoryMappedStruct(
      id = id("Bag"),
      members = List(
        IrMember(
          id = id("Bag_items"),
          name = "items",
          target = listType,
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = true,
          arrayLength = Some(2),
          endianOverride = None,
          primitiveOverride = None,
          offsetBytes = Some(0)
        )
      ),
      declaredSizeBits = Some(8)
    )

    val decoded = Json.obj(
      "items" -> Json.arr(
        Json.obj("v" -> Json.fromLong(1L)),
        Json.obj("v" -> Json.fromLong(2L))
      )
    )

    val annotated =
      HttpRouteIrEmitter.annotateDecodedAddresses(outer, decoded, 0x1000L, Some(32))

    val items = annotated.hcursor.downField("items").values.get.toVector
    assert(items(0).hcursor.downField("_address").as[String].toOption.contains("0x1000"))
    assert(items(1).hcursor.downField("_address").as[String].toOption.contains("0x1004"))
  }

  test("array codec and _address use symbol stride when larger than packed layout") {
    val item = IrType.MemoryMappedStruct(
      id = id("StrideItem"),
      members = List(
        IrMember(
          id = id("StrideItem_v"),
          name = "v",
          target = IrType.Primitive(IrPrimitive.U32),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = Some(IrPrimitive.U32),
          offsetBytes = Some(0)
        )
      ),
      declaredSizeBits = Some(4)
    )
    val listType =
      IrType.ListType(id = id("StrideList"), element = item, bytesAlias = false, bitsAlias = false)
    val output = IrType.EnclosingStruct(
      id = id("StrideOutput"),
      members = List(
        IrMember(
          id = id("StrideOutput_value"),
          name = "value",
          target = listType,
          staticAddress = Some(0x2000L),
          paddingRepeats = None,
          isPointer = false,
          isArray = true,
          arrayLength = Some(2),
          endianOverride = None,
          primitiveOverride = None,
          readSizeBytes = Some(16)
        )
      ),
      declaredSizeBits = None
    )

    val read = HttpRouteIrEmitter
      .emitRoutePlansFromIr(
        List(
          IrService(
            name = "Api",
            wordSizeBits = Some(32),
            defaultEndian = IrEndian.Big,
            operations = List(
              IrOperation(name = "GetStride", routePath = "/api/Api/GetStride", output = output)
            )
          )
        )
      )
      .routes("/api/Api/GetStride")
      .reads
      .head

    assert(read.sizeBytes == 16)
    assert(read.elementStrideBytes.contains(8))

    // Packed element is 4 bytes; each slot is padded to 8.
    val bytes = Array[Byte](
      0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00
    )
    val decoded = decode(read, bytes)
    val values = decoded.asArray.get.map(_.hcursor.downField("v").as[Long].toOption.get)
    assert(values == Vector(1L, 2L))

    val annotated =
      HttpRouteIrEmitter.annotateDecodedAddresses(
        listType,
        decoded,
        0x2000L,
        Some(32),
        read.elementStrideBytes
      )
    val elements = annotated.asArray.get
    assert(elements(0).hcursor.downField("_address").as[String].toOption.contains("0x2000"))
    assert(elements(1).hcursor.downField("_address").as[String].toOption.contains("0x2008"))
  }

  test("pointer array codec uses symbol stride when larger than pointer width") {
    val listType = IrType.ListType(
      id = id("PtrList"),
      element = IrType.Primitive(IrPrimitive.LongWord),
      bytesAlias = false,
      bitsAlias = false
    )
    val output = IrType.EnclosingStruct(
      id = id("PtrOutput"),
      members = List(
        IrMember(
          id = id("PtrOutput_value"),
          name = "value",
          target = listType,
          staticAddress = Some(0x3000L),
          paddingRepeats = None,
          isPointer = true,
          isArray = true,
          arrayLength = Some(2),
          endianOverride = None,
          primitiveOverride = None,
          readSizeBytes = Some(0x10)
        )
      ),
      declaredSizeBits = None
    )

    val route = HttpRouteIrEmitter
      .emitRoutePlansFromIr(
        List(
          IrService(
            name = "Api",
            wordSizeBits = Some(32),
            defaultEndian = IrEndian.Big,
            operations = List(
              IrOperation(
                name = "GetPtrs",
                routePath = "/api/Api/GetPtrs",
                output = output,
                pointerChain = Some(
                  IrPointerChain(
                    pointeeType = IrType.Primitive(IrPrimitive.U8),
                    pointerDepth = 1,
                    outerArrayLength = Some(2)
                  )
                )
              )
            )
          )
        )
      )
      .routes("/api/Api/GetPtrs")

    val read = route.reads.head
    assert(read.sizeBytes == 0x10)
    assert(read.elementStrideBytes.contains(8))
    assert(route.pointerChain.flatMap(_.outerElementStrideBytes).contains(8))

    // Packed pointers are 4 bytes; each slot is padded to 8.
    val bytes = Array[Byte](
      0x10, 0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00
    )
    val decoded = decode(read, bytes).asArray.get.map(_.as[Long].toOption.get)
    assert(decoded == Vector(0x10001000L, 0x10002000L))
  }
}
