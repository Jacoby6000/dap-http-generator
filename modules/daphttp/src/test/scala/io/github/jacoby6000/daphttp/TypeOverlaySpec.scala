package io.github.jacoby6000.daphttp

import cats.effect.IO
import cats.effect.Ref
import cats.effect.unsafe.implicits.global
import io.circe.Json
import io.circe.syntax._
import org.http4s.Method
import org.http4s.Request
import org.http4s.Status
import org.http4s.circe.CirceEntityCodec._
import org.http4s.implicits._
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.BitVector
import software.amazon.smithy.model.shapes.ShapeId

import java.nio.file.Files
import java.util.Base64

class TypeOverlaySpec extends AnyFunSuite {
  private def id(name: String): ShapeId = ShapeId.from(s"example#$name")

  private def member(
      name: String,
      target: IrType,
      offset: Option[Int] = None,
      primitive: Option[IrPrimitive] = None,
      isArray: Boolean = false,
      arrayLength: Option[Int] = None
  ): IrMember =
    IrMember(
      id = id(s"PadDemo_$name"),
      name = name,
      target = target,
      staticAddress = None,
      paddingRepeats = None,
      isPointer = false,
      isArray = isArray,
      arrayLength = arrayLength,
      endianOverride = None,
      primitiveOverride = primitive,
      offsetBytes = offset
    )

  /** Source layout: u16 x0 @0, then padF as u8[6] @2 (classic "padding that isn't"). */
  private val padDemo: IrType.MemoryMappedStruct = {
    val padList =
      IrType.ListType(id("PadDemo_padF_list"), IrType.Primitive(IrPrimitive.U8), false, false)
    IrType.MemoryMappedStruct(
      id = id("PadDemo"),
      members = List(
        member(
          "x0",
          IrType.Primitive(IrPrimitive.U16),
          offset = Some(0),
          primitive = Some(IrPrimitive.U16)
        ),
        member(
          "padF",
          padList,
          offset = Some(2),
          primitive = Some(IrPrimitive.U8),
          isArray = true,
          arrayLength = Some(6)
        )
      ),
      declaredSizeBytes = Some(8)
    )
  }

  private val services: List[IrService] = List(
    IrService(
      name = "Api",
      wordSizeBits = Some(32),
      defaultEndian = IrEndian.Big,
      operations = List(
        IrOperation(
          name = "GetPadDemo",
          routePath = "/api/Api/GetPadDemo",
          output = IrType.EnclosingStruct(
            id = id("GetPadDemoOutput"),
            members = List(
              member("value", padDemo).copy(
                id = id("GetPadDemoOutput_value"),
                staticAddress = Some(0x1000L),
                offsetBytes = None
              )
            ),
            declaredSizeBytes = None
          )
        )
      )
    )
  )

  test("catalog omits fields by default; fieldsFor returns descriptors") {
    val typeIndex = TypeOverlay.buildTypeIndex(services)
    assert(typeIndex.contains(id("PadDemo")))
    val entry = TypeOverlay
      .catalog(services, TypeOverlayDocument.empty)
      .find(_.id == id("PadDemo").toString)
      .get
    assert(entry.fields.isEmpty)
    assert(entry.members.contains(List("x0", "padF")))
    val fields =
      TypeOverlay.fieldsFor(services, TypeOverlayDocument.empty, id("PadDemo").toString).get
    assert(fields.map(_.name) == List("x0", "padF"))
    assert(fields.head.typeId == "u16")
    assert(!fields.head.isArray)
    assert(fields(1).typeId == "u8")
    assert(fields(1).isArray)
    assert(fields(1).arrayLength.contains(6))
  }

  test("widening a field absorbs following pad bytes when types stay naturally aligned") {
    val source = IrType.MemoryMappedStruct(
      id = id("WideDemo"),
      members = List(
        member(
          "a",
          IrType.Primitive(IrPrimitive.U8),
          offset = Some(0),
          primitive = Some(IrPrimitive.U8)
        ),
        member(
          "b",
          IrType.Primitive(IrPrimitive.U8),
          offset = Some(1),
          primitive = Some(IrPrimitive.U8)
        ),
        member(
          "c",
          IrType.Primitive(IrPrimitive.U8),
          offset = Some(2),
          primitive = Some(IrPrimitive.U8)
        ),
        member(
          "d",
          IrType.Primitive(IrPrimitive.U8),
          offset = Some(3),
          primitive = Some(IrPrimitive.U8)
        ),
        member(
          "e",
          IrType.Primitive(IrPrimitive.U32),
          offset = Some(4),
          primitive = Some(IrPrimitive.U32)
        )
      ),
      declaredSizeBytes = Some(8)
    )
    val document = TypeOverlayDocument(
      structs = Map(
        id("WideDemo").toString -> OverlayStructDef(
          List(
            OverlayMember("merged", "u32"),
            OverlayMember("e", "u32")
          )
        )
      )
    )
    val typeIndex = Map(id("WideDemo") -> (source: IrType))
    val (_, codec, sizeBytes) =
      TypeOverlay
        .compileOverlayCodec(source, document, typeIndex, IrEndian.Big, Some(32))
        .toOption
        .get
    assert(sizeBytes == 8)
    val bytes = Array[Byte](0x12, 0x34, 0x56, 0x78, 0x00, 0x00, 0x00, 0x04)
    val json = codec.decode(BitVector(bytes)).toOption.get.value
    assert(json.hcursor.downField("merged").as[Long].toOption.contains(0x12345678L))
    assert(json.hcursor.downField("e").as[Long].toOption.contains(4L))
  }

  test("u8 then pointer inserts ABI alignment padding (pointer at 0x1C)") {
    val source = IrType.MemoryMappedStruct(
      id = id("AlignDemo"),
      members = List(
        member(
          "unk18",
          IrType.Primitive(IrPrimitive.U8),
          offset = Some(0x18),
          primitive = Some(IrPrimitive.U8)
        )
      ),
      declaredSizeBytes = Some(0x20)
    )
    // Overlay rebuilds from member list only (offsets recomputed from 0).
    val document = TypeOverlayDocument(
      structs = Map(
        id("AlignDemo").toString -> OverlayStructDef(
          List(
            OverlayMember("pad0", "u8", isArray = true, arrayLength = Some(0x18)),
            OverlayMember("unk18", "u8"),
            OverlayMember("unk1C", "longWord", isPointer = true),
            OverlayMember("unk20", "longWord", isPointer = true)
          )
        )
      )
    )
    val typeIndex = Map(id("AlignDemo") -> (source: IrType))
    val (rewritten, codec, sizeBytes) =
      TypeOverlay
        .compileOverlayCodec(source, document, typeIndex, IrEndian.Big, Some(32))
        .toOption
        .get
    val struct = rewritten.asInstanceOf[IrType.Struct]
    val byName = struct.members.map(m => m.name -> m.offsetBytes).toMap
    assert(byName("unk18").contains(0x18))
    assert(byName("unk1C").contains(0x1c))
    assert(byName("unk20").contains(0x20))
    assert(sizeBytes == 0x24)
    // bytes 0x18..0x23: u8, 3 pad, ptr 0xAABBCCDD, ptr 0x11223344
    val bytes = Array.fill[Byte](0x24)(0)
    bytes(0x18) = 0x7f
    bytes(0x1c) = 0xaa.toByte
    bytes(0x1d) = 0xbb.toByte
    bytes(0x1e) = 0xcc.toByte
    bytes(0x1f) = 0xdd.toByte
    bytes(0x20) = 0x11
    bytes(0x21) = 0x22
    bytes(0x22) = 0x33
    bytes(0x23) = 0x44
    val json = codec.decode(BitVector(bytes)).toOption.get.value
    assert(json.hcursor.downField("unk18").as[Long].toOption.contains(0x7fL))
    def ptr(field: String): Long =
      json.hcursor.downField(field).as[Long].toOption.get & 0xffffffffL
    assert(ptr("unk1C") == 0xaabbccddL)
    assert(ptr("unk20") == 0x11223344L)
  }

  test("overlay rebuild assigns aligned offsets; widened prior field absorbs pad") {
    val document = TypeOverlayDocument(
      structs = Map(
        id("PadDemo").toString -> OverlayStructDef(
          List(
            OverlayMember("x0", "u32"),
            OverlayMember("tail", "u16")
          )
        )
      )
    )
    val typeIndex = TypeOverlay.buildTypeIndex(services)
    val rewritten =
      TypeOverlay.rewriteType(padDemo, document, typeIndex, Some(32)).toOption.get
    val struct = rewritten.asInstanceOf[IrType.Struct]
    assert(struct.members.map(_.name) == List("x0", "tail"))
    assert(struct.members.map(_.offsetBytes) == List(Some(0), Some(4)))
    val size = HttpRouteIrEmitter.sizeBytesForType(struct, Some(32)).toOption.get
    // sizeof rounds up to struct alignment (max member align = 4).
    assert(size == 8)
    val codec = HttpRouteIrEmitter.compileCodec(struct, IrEndian.Big, Some(32)).toOption.get
    val bytes = Array[Byte](0x12, 0x34, 0x56, 0x78, 0xab.toByte, 0xcd.toByte, 0, 0)
    val json = codec.decode(BitVector(bytes)).toOption.get.value
    assert(json.hcursor.downField("x0").as[Long].toOption.contains(0x12345678L))
    assert(json.hcursor.downField("tail").as[Long].toOption.contains(0xabcdL))
  }

  test("newStructs are resolvable as member types") {
    val document = TypeOverlayDocument(
      structs = Map(
        id("PadDemo").toString -> OverlayStructDef(
          List(OverlayMember("inner", "overlay#Pair"))
        )
      ),
      newStructs = List(
        OverlayNewStruct(
          "overlay#Pair",
          List(OverlayMember("a", "u16"), OverlayMember("b", "u16"))
        )
      )
    )
    val typeIndex = TypeOverlay.buildTypeIndex(services)
    val (rewritten, codec, sizeBytes) =
      TypeOverlay
        .compileOverlayCodec(padDemo, document, typeIndex, IrEndian.Big, Some(32))
        .toOption
        .get
    assert(sizeBytes == 4)
    val nested = rewritten.asInstanceOf[IrType.Struct].members.head.target
    assert(nested.isInstanceOf[IrType.Struct])
    val bytes = Array[Byte](0x00, 0x01, 0x00, 0x02)
    val json = codec.decode(BitVector(bytes)).toOption.get.value
    val inner = json.hcursor.downField("inner")
    assert(inner.downField("a").as[Long].toOption.contains(1L))
    assert(inner.downField("b").as[Long].toOption.contains(2L))
  }

  test("rejects unknown type ids and empty member names") {
    assert(
      TypeOverlay.validate(TypeOverlayDocument(structs = Map("x" -> OverlayStructDef(Nil)))).isLeft
    )
    assert(
      TypeOverlay
        .validate(
          TypeOverlayDocument(
            structs = Map(
              id("PadDemo").toString -> OverlayStructDef(List(OverlayMember("", "u8")))
            )
          )
        )
        .isLeft
    )
    val typeIndex = TypeOverlay.buildTypeIndex(services)
    val bad = TypeOverlayDocument(
      structs = Map(
        id("PadDemo").toString -> OverlayStructDef(List(OverlayMember("x", "notARealType")))
      )
    )
    assert(TypeOverlay.resolveTypeId("notARealType", bad, typeIndex).isLeft)
  }

  test("PUT /overlays dual-decodes data routes and persists to file") {
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(services)
    assert(plans.errors.isEmpty)
    val plansRef = Ref.unsafe[IO, RoutePlansLoadResult](plans)
    val overlaysRef =
      Ref.unsafe[IO, OverlayEngine](OverlayEngine.fromServices(TypeOverlayDocument.empty, services))
    val persistPath = Files.createTempFile("dap-overlays", ".json")
    persistPath.toFile.deleteOnExit()

    val memory = (0 until 8).map { i =>
      (0x1000L + i) -> (0x10 + i).toByte
    }.toMap

    val dapClient = new DapHttpServerMain.DapClient {
      override def readMemory(address: Long, sizeBytes: Int): IO[Either[String, String]] =
        IO {
          val bytes = (0 until sizeBytes).map { i =>
            memory.getOrElse(address + i, 0.toByte)
          }.toArray
          Right(Base64.getEncoder.encodeToString(bytes))
        }
      override def continueExecution(threadId: Option[Int] = None): IO[Either[String, Json]] = {
        val _ = threadId
        IO.pure(Right(Json.obj()))
      }
    }

    val app =
      DapHttpServerMain.routes(plansRef, dapClient, overlaysRef, Some(persistPath)).orNotFound

    val typesBody =
      app
        .run(Request[IO](Method.GET, uri"/types"))
        .unsafeRunSync()
        .bodyText
        .compile
        .string
        .unsafeRunSync()
    assert(typesBody.contains("\"u8\""))
    assert(typesBody.contains("example#PadDemo"))
    assert(!typesBody.contains("\"fields\""))

    val fieldsBody =
      app
        .run(
          Request[IO](Method.GET, uri"/types/fields".withQueryParam("id", id("PadDemo").toString))
        )
        .unsafeRunSync()
        .bodyText
        .compile
        .string
        .unsafeRunSync()
    assert(fieldsBody.contains("\"x0\""))
    assert(fieldsBody.contains("\"padF\""))

    val overlayDoc = TypeOverlayDocument(
      structs = Map(
        id("PadDemo").toString -> OverlayStructDef(
          List(
            OverlayMember("wide", "u32"),
            OverlayMember("rest", "u16")
          )
        )
      )
    )
    val putResponse = app
      .run(
        Request[IO](Method.PUT, uri"/overlays")
          .withEntity(overlayDoc.asJson)
      )
      .unsafeRunSync()
    assert(putResponse.status == Status.Ok)

    val loaded = TypeOverlayDocument.load(persistPath).toOption.get
    assert(loaded.structs.contains(id("PadDemo").toString))

    val getResponse =
      app.run(Request[IO](Method.GET, uri"/api/Api/GetPadDemo")).unsafeRunSync()
    assert(getResponse.status == Status.Ok)
    val body =
      getResponse.body.compile.toVector.unsafeRunSync().map(_.toChar).mkString
    val json = io.circe.parser.parse(body).toOption.get
    val read = json.hcursor.downField("reads").downN(0)
    assert(read.downField("decoded").focus.isDefined)
    val overlayDecoded = read.downField("overlayDecoded")
    assert(overlayDecoded.focus.isDefined)
    // Source keeps padF; overlay uses wide/rest after layout rebuild.
    assert(read.downField("decoded").downField("padF").focus.isDefined)
    assert(overlayDecoded.downField("wide").focus.isDefined)
    assert(overlayDecoded.downField("rest").focus.isDefined)
    assert(overlayDecoded.downField("padF").focus.isEmpty)

    val badPut = app
      .run(
        Request[IO](Method.PUT, uri"/overlays").withEntity(
          TypeOverlayDocument(
            structs = Map(
              id("PadDemo").toString -> OverlayStructDef(List(OverlayMember("x", "nope")))
            )
          ).asJson
        )
      )
      .unsafeRunSync()
    assert(badPut.status == Status.BadRequest)
  }
}
