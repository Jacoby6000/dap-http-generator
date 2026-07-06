package io.github.jacoby6000.daphttp

import software.amazon.smithy.model.shapes.ShapeId

final case class IrService(name: String, wordSizeBits: Option[Int], operations: List[IrOperation])
final case class IrOperation(name: String, routePath: String, output: IrType.Struct)
final case class IrMember(
    id: ShapeId,
    name: String,
    target: IrType,
    staticAddress: Option[Long],
    cStringBytes: Option[Int],
    paddingRepeats: Option[Int],
    isPointer: Boolean,
    isArray: Boolean,
    arrayLength: Option[Int],
    primitiveOverride: Option[IrPrimitive]
)

sealed trait IrPrimitive
object IrPrimitive {
  case object U8 extends IrPrimitive
  case object S8 extends IrPrimitive
  case object U16 extends IrPrimitive
  case object S16 extends IrPrimitive
  case object U32 extends IrPrimitive
  case object S32 extends IrPrimitive
  case object Bool extends IrPrimitive
  case object LongWord extends IrPrimitive
}

sealed trait IrType
object IrType {
  final case class Struct(
      id: ShapeId,
      members: List[IrMember],
      isDapStruct: Boolean,
      isBitmask: Boolean,
      declaredSizeBits: Option[Int]
  ) extends IrType
  final case class Union(id: ShapeId, members: List[IrMember]) extends IrType
  final case class ListType(id: ShapeId, element: IrType, bytesAlias: Boolean, bitsAlias: Boolean)
      extends IrType
  final case class MapType(id: ShapeId, key: IrType, value: IrType) extends IrType
  final case class Primitive(kind: IrPrimitive) extends IrType
  final case class Ref(id: ShapeId) extends IrType
}
