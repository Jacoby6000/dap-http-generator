package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite
import software.amazon.smithy.model.shapes.ShapeId

class IrLayoutSpec extends AnyFunSuite {
  private def id(name: String): ShapeId = ShapeId.from(s"example#$name")

  private def member(
      name: String,
      target: IrType,
      primitive: Option[IrPrimitive] = None,
      isPointer: Boolean = false,
      unionGroup: Option[String] = None,
      layoutBitWidth: Option[Int] = None
  ): IrMember =
    IrMember(
      id = id(s"S_$name"),
      name = name,
      target = target,
      staticAddress = None,
      paddingRepeats = None,
      isPointer = isPointer,
      isArray = false,
      arrayLength = None,
      endianOverride = None,
      primitiveOverride = primitive,
      layoutBitWidth = layoutBitWidth,
      unionGroup = unionGroup,
      offsetBytes = None
    )

  test("u8 then pointer aligns pointer to 0x1C after pad bytes at 0x18") {
    val padList =
      IrType.ListType(id("pad_list"), IrType.Primitive(IrPrimitive.U8), false, false)
    val members = List(
      member(
        "pad0",
        padList,
        primitive = Some(IrPrimitive.U8)
      ).copy(isArray = true, arrayLength = Some(0x18)),
      member("unk18", IrType.Primitive(IrPrimitive.U8), Some(IrPrimitive.U8)),
      member("unk1C", IrType.Primitive(IrPrimitive.LongWord), isPointer = true),
      member("unk20", IrType.Primitive(IrPrimitive.LongWord), isPointer = true)
    )
    val (packed, sizeof) = IrLayout.packMembers(members, Some(32)).toOption.get
    val byName = packed.map(m => m.name -> m.offsetBytes).toMap
    assert(byName("unk18").contains(0x18))
    assert(byName("unk1C").contains(0x1c))
    assert(byName("unk20").contains(0x20))
    assert(sizeof == 0x24)
  }

  test("union alternatives share one offset and advance by max size") {
    val members = List(
      member("tag", IrType.Primitive(IrPrimitive.U32), Some(IrPrimitive.U32)),
      member(
        "asU32",
        IrType.Primitive(IrPrimitive.U32),
        Some(IrPrimitive.U32),
        unionGroup = Some("u")
      ),
      member(
        "asU64",
        IrType.Primitive(IrPrimitive.U64),
        Some(IrPrimitive.U64),
        unionGroup = Some("u")
      ),
      member("tail", IrType.Primitive(IrPrimitive.U8), Some(IrPrimitive.U8))
    )
    val (packed, sizeof) = IrLayout.packMembers(members, Some(32)).toOption.get
    val byName = packed.map(m => m.name -> m.offsetBytes.get).toMap
    assert(byName("tag") == 0)
    // After u32 tag, u64-aligned union slot starts at 8.
    assert(byName("asU32") == 8)
    assert(byName("asU64") == 8)
    assert(byName("tail") == 16)
    assert(sizeof == 24) // end 17, round up to max member align 8
  }

  test("bitmask storage uses layoutBitWidth for size and alignment") {
    val members = List(
      member("flags", IrType.Primitive(IrPrimitive.U8), layoutBitWidth = Some(16)),
      member("next", IrType.Primitive(IrPrimitive.U32), Some(IrPrimitive.U32))
    )
    val (packed, sizeof) = IrLayout.packMembers(members, Some(32)).toOption.get
    assert(packed.head.offsetBytes.contains(0))
    assert(packed(1).offsetBytes.contains(4)) // 2-byte bitmask, align u32 to 4
    assert(sizeof == 8)
  }

  test("sizeof rounds up to struct alignment") {
    val members = List(
      member("a", IrType.Primitive(IrPrimitive.U32), Some(IrPrimitive.U32)),
      member("b", IrType.Primitive(IrPrimitive.U16), Some(IrPrimitive.U16))
    )
    val (_, sizeof) = IrLayout.packMembers(members, Some(32)).toOption.get
    assert(sizeof == 8)
  }

  test("validateCommentOffsets warns on mismatch") {
    val members = List(
      member("a", IrType.Primitive(IrPrimitive.U8), Some(IrPrimitive.U8))
        .copy(offsetBytes = Some(0)),
      member("b", IrType.Primitive(IrPrimitive.U32), Some(IrPrimitive.U32))
        .copy(offsetBytes = Some(4))
    )
    val warnings = IrLayout.validateCommentOffsets(
      "Padded",
      members,
      Map(("Padded", "a") -> 0, ("Padded", "b") -> 0x08)
    )
    assert(warnings.size == 1)
    assert(warnings.head.contains("Padded.b"))
    assert(warnings.head.contains("0x8"))
    assert(warnings.head.contains("0x4"))
  }
}
