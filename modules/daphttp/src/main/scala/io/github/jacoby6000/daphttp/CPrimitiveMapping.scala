package io.github.jacoby6000.daphttp

/** C / doldecomp type-name spellings → [[IrPrimitive]].
  *
  * DESNOTE(jbarber, 2026-07-21): Kept separate from [[DapSmithyTraits]] so Smithy contracts do not
  * absorb C aliases (`unsigned int`, `void*`, …). Overlay UI aliases live in
  * [[IrPrimitiveAliases]].
  */
object CPrimitiveMapping {
  def fromTypeName(typeName: String): Option[IrPrimitive] = {
    val normalized = CHeaderParser.normalizeTypeName(typeName).replaceAll("\\s+", "")
    normalized match {
      case "u8"                          => Some(IrPrimitive.U8)
      case "s8"                          => Some(IrPrimitive.S8)
      case "u16"                         => Some(IrPrimitive.U16)
      case "s16"                         => Some(IrPrimitive.S16)
      case "u32"                         => Some(IrPrimitive.U32)
      case "s32"                         => Some(IrPrimitive.S32)
      case "u64"                         => Some(IrPrimitive.U64)
      case "s64"                         => Some(IrPrimitive.S64)
      case "u128"                        => Some(IrPrimitive.U128)
      case "s128"                        => Some(IrPrimitive.S128)
      case "f32"                         => Some(IrPrimitive.F32)
      case "f64"                         => Some(IrPrimitive.F64)
      case "char"                        => Some(IrPrimitive.Char)
      case "bool"                        => Some(IrPrimitive.Bool)
      case "byte"                        => Some(IrPrimitive.S8)
      case "short"                       => Some(IrPrimitive.S16)
      case "int"                         => Some(IrPrimitive.S32)
      case "long"                        => Some(IrPrimitive.LongWord)
      case "float"                       => Some(IrPrimitive.F32)
      case "double"                      => Some(IrPrimitive.F64)
      case "unsignedchar"                => Some(IrPrimitive.U8)
      case "signedchar"                  => Some(IrPrimitive.S8)
      case "unsignedshort"               => Some(IrPrimitive.U16)
      case "signedshort"                 => Some(IrPrimitive.S16)
      case "unsignedint"                 => Some(IrPrimitive.U32)
      case "signedint"                   => Some(IrPrimitive.S32)
      case "unsigned"                    => Some(IrPrimitive.U32)
      case "signed"                      => Some(IrPrimitive.S32)
      case "unsignedlong"                => Some(IrPrimitive.LongWord)
      case "signedlong"                  => Some(IrPrimitive.LongWord)
      case "longlong"                    => Some(IrPrimitive.S64)
      case "unsignedlonglong"            => Some(IrPrimitive.U64)
      case "signedlonglong"              => Some(IrPrimitive.S64)
      case "unsignedshortint"            => Some(IrPrimitive.U16)
      case "signedshortint"              => Some(IrPrimitive.S16)
      case "unsignedlongint"             => Some(IrPrimitive.LongWord)
      case "signedlongint"               => Some(IrPrimitive.LongWord)
      case "longlongint"                 => Some(IrPrimitive.S64)
      case "unsignedlonglongint"         => Some(IrPrimitive.U64)
      case "signedlonglongint"           => Some(IrPrimitive.S64)
      case "void"                        => Some(IrPrimitive.LongWord)
      case _ if normalized.endsWith("*") =>
        Some(IrPrimitive.LongWord)
      case _ => None
    }
  }
}
