package io.github.jacoby6000.daphttp

import org.scalatest.Assertions

object IrEquivalence extends Assertions {
  final case class NormalizedModel(services: List[NormalizedService])

  final case class NormalizedService(
      name: String,
      wordSizeBits: Option[Int],
      defaultEndian: IrEndian,
      operations: List[NormalizedOperation]
  )

  final case class NormalizedOperation(
      name: String,
      routePath: String,
      output: NormalizedStruct
  )

  final case class NormalizedStruct(
      kind: String,
      name: String,
      declaredSizeBits: Option[Int],
      members: List[NormalizedMember]
  )

  final case class NormalizedMember(
      name: String,
      value: NormalizedValue,
      staticAddress: Option[Long],
      paddingRepeats: Option[Int],
      isPointer: Boolean,
      isArray: Boolean,
      arrayLength: Option[Int],
      endianOverride: Option[IrEndian]
  )

  sealed trait NormalizedValue
  final case class NormalizedScalar(width: IrPrimitive) extends NormalizedValue
  final case class NormalizedStructType(structure: NormalizedStruct) extends NormalizedValue
  final case class NormalizedUnion(name: String, members: List[NormalizedMember])
      extends NormalizedValue
  final case class NormalizedList(
      name: String,
      element: NormalizedValue,
      bytesAlias: Boolean,
      bitsAlias: Boolean
  ) extends NormalizedValue
  final case class NormalizedMap(name: String, key: NormalizedValue, value: NormalizedValue)
      extends NormalizedValue
  final case class NormalizedRef(name: String) extends NormalizedValue
  final case class NormalizedIntEnum(
      name: String,
      values: List[(String, Int)],
      underlying: IrPrimitive
  ) extends NormalizedValue
  final case class NormalizedFunctionPointer(
      name: String,
      params: List[(String, String)],
      returnType: String
  ) extends NormalizedValue

  def normalize(services: List[IrService]): NormalizedModel =
    NormalizedModel(services.map(normalizeService).sortBy(_.name))

  def assertEquivalent(expected: List[IrService], actual: List[IrService]): Unit = {
    val _ = assert(normalize(expected) == normalize(actual))
  }

  private def normalizeService(service: IrService): NormalizedService =
    NormalizedService(
      name = service.name,
      wordSizeBits = service.wordSizeBits,
      defaultEndian = service.defaultEndian,
      operations = service.operations.map(normalizeOperation).sortBy(_.name)
    )

  private def normalizeOperation(operation: IrOperation): NormalizedOperation =
    NormalizedOperation(
      name = operation.name,
      routePath = operation.routePath,
      output = normalizeStruct(operation.output)
    )

  private def normalizeStruct(struct: IrType.Struct): NormalizedStruct =
    NormalizedStruct(
      kind = structKind(struct),
      name = struct.id.getName,
      declaredSizeBits = struct.declaredSizeBits,
      members = struct.members.map(normalizeMember)
    )

  private def normalizeMember(member: IrMember): NormalizedMember =
    NormalizedMember(
      name = member.name,
      value = normalizeValue(member),
      staticAddress = member.staticAddress,
      paddingRepeats = member.paddingRepeats,
      isPointer = member.isPointer,
      isArray = member.isArray,
      arrayLength = member.arrayLength,
      endianOverride = member.endianOverride
    )

  private def normalizeValue(member: IrMember): NormalizedValue =
    member.target match {
      case fp: IrType.FunctionPointer =>
        NormalizedFunctionPointer(
          name = fp.name,
          params = fp.params.map(p => (p.typeName, p.name)),
          returnType = fp.returnType
        )
      case _ if member.isPointer =>
        NormalizedScalar(member.primitiveOverride.getOrElse(IrPrimitive.LongWord))
      case _ =>
        member.target match {
          case IrType.Primitive(kind) =>
            NormalizedScalar(member.primitiveOverride.getOrElse(kind))
          case struct: IrType.Struct =>
            NormalizedStructType(normalizeStruct(struct))
          case union: IrType.Union =>
            NormalizedUnion(
              name = union.id.getName,
              members = union.members.map(normalizeMember)
            )
          case listType: IrType.ListType =>
            NormalizedList(
              name = listType.id.getName,
              element = normalizeTypeValue(listType.element),
              bytesAlias = listType.bytesAlias,
              bitsAlias = listType.bitsAlias
            )
          case mapType: IrType.MapType =>
            NormalizedMap(
              name = mapType.id.getName,
              key = normalizeTypeValue(mapType.key),
              value = normalizeTypeValue(mapType.value)
            )
          case intEnum: IrType.IntEnum =>
            NormalizedIntEnum(
              name = intEnum.id.getName,
              values = intEnum.values.map(v => v.name -> v.value),
              underlying = intEnum.underlying
            )
          case IrType.Ref(id) =>
            NormalizedRef(id.getName)
          case _: IrType.FunctionPointer =>
            throw new IllegalStateException("FunctionPointer should have been handled earlier")
        }
    }

  private def normalizeTypeValue(irType: IrType): NormalizedValue =
    irType match {
      case struct: IrType.Struct =>
        NormalizedStructType(normalizeStruct(struct))
      case union: IrType.Union =>
        NormalizedUnion(
          name = union.id.getName,
          members = union.members.map(normalizeMember)
        )
      case listType: IrType.ListType =>
        NormalizedList(
          name = listType.id.getName,
          element = normalizeTypeValue(listType.element),
          bytesAlias = listType.bytesAlias,
          bitsAlias = listType.bitsAlias
        )
      case mapType: IrType.MapType =>
        NormalizedMap(
          name = mapType.id.getName,
          key = normalizeTypeValue(mapType.key),
          value = normalizeTypeValue(mapType.value)
        )
      case intEnum: IrType.IntEnum =>
        NormalizedIntEnum(
          name = intEnum.id.getName,
          values = intEnum.values.map(v => v.name -> v.value),
          underlying = intEnum.underlying
        )
      case IrType.Ref(id) =>
        NormalizedRef(id.getName)
      case IrType.Primitive(kind) =>
        NormalizedScalar(kind)
      case fp: IrType.FunctionPointer =>
        NormalizedFunctionPointer(
          name = fp.name,
          params = fp.params.map(p => (p.typeName, p.name)),
          returnType = fp.returnType
        )
    }

  private def structKind(struct: IrType.Struct): String =
    struct match {
      case _: IrType.Bitmask            => "bitmask"
      case _: IrType.MemoryMappedStruct => "memoryMapped"
      case _: IrType.EnclosingStruct    => "enclosing"
    }
}
