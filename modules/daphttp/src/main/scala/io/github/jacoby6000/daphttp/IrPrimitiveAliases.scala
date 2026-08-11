package io.github.jacoby6000.daphttp

/** Canonical and synonym aliases for [[IrPrimitive]] used by overlays / type catalog UI. */
object IrPrimitiveAliases {
  def canonical(kind: IrPrimitive): String =
    kind match {
      case IrPrimitive.U8       => "u8"
      case IrPrimitive.S8       => "s8"
      case IrPrimitive.U16      => "u16"
      case IrPrimitive.S16      => "s16"
      case IrPrimitive.U32      => "u32"
      case IrPrimitive.S32      => "s32"
      case IrPrimitive.U64      => "u64"
      case IrPrimitive.S64      => "s64"
      case IrPrimitive.U128     => "u128"
      case IrPrimitive.S128     => "s128"
      case IrPrimitive.F8       => "f8"
      case IrPrimitive.F16      => "f16"
      case IrPrimitive.F32      => "f32"
      case IrPrimitive.F64      => "f64"
      case IrPrimitive.Char     => "char"
      case IrPrimitive.Bool     => "bool"
      case IrPrimitive.LongWord => "longWord"
    }

  private val ByAlias: Map[String, IrPrimitive] = {
    import IrPrimitive._
    Map(
      "u8" -> U8,
      "uint8" -> U8,
      "s8" -> S8,
      "int8" -> S8,
      "u16" -> U16,
      "uint16" -> U16,
      "s16" -> S16,
      "int16" -> S16,
      "u32" -> U32,
      "uint32" -> U32,
      "s32" -> S32,
      "int32" -> S32,
      "u64" -> U64,
      "uint64" -> U64,
      "s64" -> S64,
      "int64" -> S64,
      "u128" -> U128,
      "s128" -> S128,
      "f8" -> F8,
      "f16" -> F16,
      "f32" -> F32,
      "float" -> F32,
      "f64" -> F64,
      "double" -> F64,
      "char" -> Char,
      "bool" -> Bool,
      "boolean" -> Bool,
      "longword" -> LongWord,
      "pointer" -> LongWord
    )
  }

  def fromAlias(raw: String): Option[IrPrimitive] =
    ByAlias.get(raw.trim.toLowerCase)

  def isKnown(raw: String): Boolean =
    fromAlias(raw).isDefined

  def catalogAliases: List[String] =
    ByAlias.keys.toList.sorted.distinct
}
