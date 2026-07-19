package io.github.jacoby6000.daphttp

import software.amazon.smithy.model.shapes.ShapeId

sealed trait IrEndian
object IrEndian {
  case object Big extends IrEndian
  case object Little extends IrEndian
}

final case class IrService(
    name: String,
    wordSizeBits: Option[Int],
    defaultEndian: IrEndian,
    operations: List[IrOperation]
)
final case class IrOperation(
    name: String,
    routePath: String,
    output: IrType.Struct,
    pointerChain: Option[IrPointerChain] = None
)
final case class IrPointerChain(
    pointeeType: IrType,
    pointerDepth: Int,
    outerArrayLength: Option[Int],
    followCString: Boolean = false
)
final case class IrMember(
    id: ShapeId,
    name: String,
    target: IrType,
    staticAddress: Option[Long],
    paddingRepeats: Option[Int],
    isPointer: Boolean,
    isArray: Boolean,
    arrayLength: Option[Int],
    endianOverride: Option[IrEndian],
    primitiveOverride: Option[IrPrimitive],
    readSizeBytes: Option[Int] = None,
    unionGroup: Option[String] = None,
    layoutBitWidth: Option[Int] = None,
    offsetBytes: Option[Int] = None
)

sealed trait IrPrimitive
object IrPrimitive {
  case object U8 extends IrPrimitive
  case object S8 extends IrPrimitive
  case object U16 extends IrPrimitive
  case object S16 extends IrPrimitive
  case object U32 extends IrPrimitive
  case object S32 extends IrPrimitive
  case object U64 extends IrPrimitive
  case object S64 extends IrPrimitive
  case object U128 extends IrPrimitive
  case object S128 extends IrPrimitive
  case object F8 extends IrPrimitive
  case object F16 extends IrPrimitive
  case object F32 extends IrPrimitive
  case object F64 extends IrPrimitive
  case object Char extends IrPrimitive
  case object Bool extends IrPrimitive
  case object LongWord extends IrPrimitive
}

final case class IrEnumValue(name: String, value: Int)

sealed trait IrType
object IrType {
  sealed trait Struct extends IrType {
    def id: ShapeId
    def members: List[IrMember]
    def declaredSizeBits: Option[Int]
  }
  final case class Bitmask(
      id: ShapeId,
      members: List[IrMember],
      declaredSizeBits: Option[Int]
  ) extends Struct
  final case class MemoryMappedStruct(
      id: ShapeId,
      members: List[IrMember],
      declaredSizeBits: Option[Int]
  ) extends Struct
  final case class EnclosingStruct(
      id: ShapeId,
      members: List[IrMember],
      declaredSizeBits: Option[Int]
  ) extends Struct
  final case class Union(id: ShapeId, members: List[IrMember]) extends IrType
  final case class ListType(id: ShapeId, element: IrType, bytesAlias: Boolean, bitsAlias: Boolean)
      extends IrType
  final case class MapType(id: ShapeId, key: IrType, value: IrType) extends IrType
  // DESNOTE(jbarber, 2026-07-19): C enums and Smithy intEnum shapes are int-backed
  // (typically s32). Decoded JSON uses the enumerator name when the raw value matches,
  // otherwise a hex literal `0xN` so unknown/out-of-range values stay readable.
  final case class IntEnum(
      id: ShapeId,
      values: List[IrEnumValue],
      underlying: IrPrimitive = IrPrimitive.S32
  ) extends IrType
  final case class Primitive(kind: IrPrimitive) extends IrType
  final case class Ref(id: ShapeId) extends IrType
  final case class FunctionPointer(
      name: String,
      params: List[FunctionPointerParam],
      returnType: String
  ) extends IrType
}

final case class FunctionPointerParam(typeName: String, name: String)
