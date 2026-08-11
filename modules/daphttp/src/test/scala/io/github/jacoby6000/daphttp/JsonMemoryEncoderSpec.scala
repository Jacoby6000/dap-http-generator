package io.github.jacoby6000.daphttp

import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite
import software.amazon.smithy.model.shapes.ShapeId

class JsonMemoryEncoderSpec extends AnyFunSuite {
  private def id(name: String): ShapeId = ShapeId.from(s"example#$name")

  test("encodes big-endian u32") {
    val bytes = JsonMemoryEncoder
      .encode(
        IrType.Primitive(IrPrimitive.U32),
        Json.fromLong(0x01020304L),
        IrEndian.Big,
        Some(32)
      )
      .fold(fail(_), identity)
    assert(bytes.toSeq == Seq(0x01, 0x02, 0x03, 0x04).map(_.toByte))
  }

  test("encodes little-endian u16") {
    val bytes = JsonMemoryEncoder
      .encode(
        IrType.Primitive(IrPrimitive.U16),
        Json.fromLong(0xabcdL),
        IrEndian.Little,
        Some(32)
      )
      .fold(fail(_), identity)
    assert(bytes.toSeq == Seq(0xcd, 0xab).map(_.toByte))
  }

  test("encodes enum by name") {
    val intEnum = IrType.IntEnum(
      id = id("Color"),
      underlying = IrPrimitive.S32,
      values = List(IrEnumValue("RED", 1), IrEnumValue("BLUE", 2))
    )
    val bytes = JsonMemoryEncoder
      .encode(intEnum, Json.fromString("BLUE"), IrEndian.Big, Some(32))
      .fold(fail(_), identity)
    assert(bytes.toSeq == Seq(0, 0, 0, 2).map(_.toByte))
  }

  test("resolves nested field offset") {
    val inner = IrType.MemoryMappedStruct(
      id = id("Vec"),
      members = List(
        IrMember(
          id = id("Vec_x"),
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
          id = id("Vec_y"),
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
      declaredSizeBytes = Some(8)
    )
    val outer = IrType.MemoryMappedStruct(
      id = id("Outer"),
      members = List(
        IrMember(
          id = id("Outer_pos"),
          name = "pos",
          target = inner,
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = None,
          offsetBytes = Some(0x10)
        )
      ),
      declaredSizeBytes = Some(0x18)
    )
    val (_, _, offset) =
      JsonMemoryEncoder.resolveLeaf(outer, List("pos", "y"), Some(32)).fold(fail(_), identity)
    assert(offset == 0x14)
  }
}
