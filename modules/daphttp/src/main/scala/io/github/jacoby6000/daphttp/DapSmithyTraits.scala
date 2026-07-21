package io.github.jacoby6000.daphttp

import software.amazon.smithy.model.shapes.ShapeId

/** Smithy trait ids and primitive↔trait/prelude mappings for DAP HTTP models.
  *
  * DESNOTE(jbarber, 2026-07-21): Trait contracts live here so SmithyIrGenerator and SmithyIrEmitter
  * cannot drift. C spellings belong in [[CPrimitiveMapping]]; UI overlay aliases belong in
  * [[IrPrimitiveAliases]].
  */
object DapSmithyTraits {
  val Namespace: String = "com.jacoby6000.daphttp"

  def traitId(name: String): ShapeId = ShapeId.from(s"$Namespace#$name")

  val DapStruct: ShapeId = traitId("dapStruct")
  val Bitmask: ShapeId = traitId("bitmask")
  val Size: ShapeId = traitId("size")
  val Padding: ShapeId = traitId("padding")
  val Pointer: ShapeId = traitId("pointer")
  val Array: ShapeId = traitId("array")
  val Length: ShapeId = traitId("length")
  val Endian: ShapeId = traitId("endian")
  val WordSize: ShapeId = traitId("wordSize")
  val StaticAddress: ShapeId = traitId("staticAddress")
  val PointerDepth: ShapeId = traitId("pointerDepth")
  val OuterArrayLength: ShapeId = traitId("outerArrayLength")
  val FollowCString: ShapeId = traitId("followCString")
  val PointeeShape: ShapeId = traitId("pointeeShape")
  val FunctionPointer: ShapeId = traitId("functionPointer")
  val Http: ShapeId = ShapeId.from("smithy.api#http")
  val Bytes: ShapeId = traitId("Bytes")
  val Bits: ShapeId = traitId("Bits")

  final case class WidthTrait(name: String, primitive: IrPrimitive) {
    val id: ShapeId = traitId(name)
  }

  val WidthTraits: List[WidthTrait] = List(
    WidthTrait("u8", IrPrimitive.U8),
    WidthTrait("s8", IrPrimitive.S8),
    WidthTrait("u16", IrPrimitive.U16),
    WidthTrait("s16", IrPrimitive.S16),
    WidthTrait("u32", IrPrimitive.U32),
    WidthTrait("s32", IrPrimitive.S32),
    WidthTrait("u64", IrPrimitive.U64),
    WidthTrait("s64", IrPrimitive.S64),
    WidthTrait("u128", IrPrimitive.U128),
    WidthTrait("s128", IrPrimitive.S128),
    WidthTrait("f8", IrPrimitive.F8),
    WidthTrait("f16", IrPrimitive.F16),
    WidthTrait("f32", IrPrimitive.F32),
    WidthTrait("f64", IrPrimitive.F64),
    WidthTrait("char", IrPrimitive.Char)
  )

  val PrimitiveByTraitId: Map[ShapeId, IrPrimitive] =
    WidthTraits.map(t => t.id -> t.primitive).toMap

  private val TraitNameByPrimitive: Map[IrPrimitive, String] =
    WidthTraits.map(t => t.primitive -> t.name).toMap

  /** Trait local name for emit (`@u32`, …). Bool/LongWord have no width trait. */
  def traitNameFor(kind: IrPrimitive): Option[String] =
    TraitNameByPrimitive.get(kind)

  def preludeShapeId(kind: IrPrimitive): ShapeId =
    ShapeId.from(s"smithy.api#${preludeName(kind)}")

  def preludeName(kind: IrPrimitive): String =
    kind match {
      case IrPrimitive.Bool                => "Boolean"
      case IrPrimitive.Char                => "Byte"
      case IrPrimitive.U8 | IrPrimitive.S8 =>
        "Byte"
      case IrPrimitive.U16 | IrPrimitive.S16 | IrPrimitive.U32 | IrPrimitive.S32 =>
        "Integer"
      case IrPrimitive.U64 | IrPrimitive.S64 | IrPrimitive.U128 | IrPrimitive.S128 |
          IrPrimitive.LongWord =>
        "Long"
      case IrPrimitive.F8 | IrPrimitive.F16 | IrPrimitive.F32 => "Float"
      case IrPrimitive.F64                                    => "Double"
    }
}
