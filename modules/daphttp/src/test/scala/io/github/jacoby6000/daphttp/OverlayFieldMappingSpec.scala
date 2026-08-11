package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite
import software.amazon.smithy.model.shapes.ShapeId

class OverlayFieldMappingSpec extends AnyFunSuite {
  private def id(name: String): ShapeId = ShapeId.from(s"test#$name")

  private def member(
      name: String,
      target: IrType,
      offset: Option[Int] = None
  ): IrMember =
    IrMember(
      id = id(s"M_$name"),
      name = name,
      target = target,
      staticAddress = None,
      paddingRepeats = None,
      isPointer = false,
      isArray = false,
      arrayLength = None,
      endianOverride = None,
      primitiveOverride = None,
      offsetBytes = offset
    )

  test("overlappingOverlaySpansInRange maps source bytes to overlay fields") {
    val overlay = IrType.MemoryMappedStruct(
      id = id("Ov"),
      members = List(
        member("ab", IrType.Primitive(IrPrimitive.U64)),
        member("tail", IrType.Primitive(IrPrimitive.U16))
      ),
      declaredSizeBytes = None
    )
    // a@0 size4, b@4 size4, c@8 size2 → ab@0 size8 overlaps a and b; tail@8 overlaps c
    val overA = OverlayFieldMapping.overlappingOverlaySpansInRange(overlay, 0, 4, Some(32))
    assert(overA.map(_.map(_.name)).contains(List("ab")))

    val overB = OverlayFieldMapping.overlappingOverlaySpansInRange(overlay, 4, 4, Some(32))
    assert(overB.map(_.map(_.name)).contains(List("ab")))

    val overC = OverlayFieldMapping.overlappingOverlaySpansInRange(overlay, 8, 2, Some(32))
    assert(overC.map(_.map(_.name)).contains(List("tail")))
  }

  test("overlappingOverlaySpans by member name") {
    val source = IrType.MemoryMappedStruct(
      id = id("Src"),
      members = List(
        member("left", IrType.Primitive(IrPrimitive.U32)),
        member("right", IrType.Primitive(IrPrimitive.U32))
      ),
      declaredSizeBytes = None
    )
    val overlay = IrType.MemoryMappedStruct(
      id = id("Ov"),
      members = List(
        member("wide", IrType.Primitive(IrPrimitive.U64))
      ),
      declaredSizeBytes = None
    )
    val hits =
      OverlayFieldMapping.overlappingOverlaySpans(source, overlay, "left", Some(32))
    assert(hits.map(_.map(_.name)).contains(List("wide")))
  }

  test("unionRange covers min start through max end") {
    assert(OverlayFieldMapping.unionRange(List((4, 4), (0, 4), (8, 2))).contains((0, 10)))
  }
}
