package io.github.jacoby6000.daphttp

import software.amazon.smithy.model.Model
import software.amazon.smithy.model.node.Node
import software.amazon.smithy.model.shapes.IntEnumShape
import software.amazon.smithy.model.shapes.ListShape
import software.amazon.smithy.model.shapes.MapShape
import software.amazon.smithy.model.shapes.MemberShape
import software.amazon.smithy.model.shapes.OperationShape
import software.amazon.smithy.model.shapes.ServiceShape
import software.amazon.smithy.model.shapes.Shape
import software.amazon.smithy.model.shapes.ShapeId
import software.amazon.smithy.model.shapes.SmithyIdlModelSerializer
import software.amazon.smithy.model.shapes.StructureShape
import software.amazon.smithy.model.shapes.UnionShape
import software.amazon.smithy.model.traits.DynamicTrait
import software.amazon.smithy.model.traits.Trait

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.function.Consumer
import scala.jdk.CollectionConverters._

object SmithyIrEmitter {
  private val TraitsNamespace = "com.jacoby6000.daphttp"
  private val TraitsPath = Paths.get("src/main/smithy/dap-http-traits.smithy")
  private val BytesShapeId = ShapeId.from(s"$TraitsNamespace#Bytes")
  private val BitsShapeId = ShapeId.from(s"$TraitsNamespace#Bits")

  private final case class CollectionState(
      shapes: List[(ShapeId, IrType)] = Nil,
      errors: List[String] = Nil,
      visiting: Set[ShapeId] = Set.empty
  ) {
    def shapeIds: Set[ShapeId] = shapes.map(_._1).toSet

    def withError(message: String): CollectionState =
      copy(errors = message :: errors)

    def withShape(id: ShapeId, irType: IrType): CollectionState =
      if (shapeIds.contains(id)) this else copy(shapes = shapes :+ (id -> irType))

    def withVisiting(id: ShapeId): CollectionState =
      copy(visiting = visiting + id)

    def withoutVisiting(id: ShapeId): CollectionState =
      copy(visiting = visiting - id)
  }

  def emit(services: List[IrService]): Either[List[String], String] =
    buildModel(services).map(serializeModel)

  def emitToPath(services: List[IrService], outputPath: Path): Either[List[String], Unit] =
    emit(services).map { content =>
      val parent = outputPath.getParent
      if (parent != null) {
        Files.createDirectories(parent)
      }
      val _ = Files.writeString(outputPath, content)
      ()
    }

  def buildModel(services: List[IrService]): Either[List[String], Model] =
    if (services.isEmpty) {
      Left(List("At least one IR service is required to emit Smithy."))
    } else {
      val namespaces = services.flatMap(_.operations.map(_.output.id.getNamespace)).toSet
      collectShapes(services) match {
        case state if state.errors.nonEmpty =>
          Left(state.errors.distinct)
        case state =>
          val shapes = state.shapes.flatMap { case (id, irType) => buildShape(id, irType) }
          val operations = services.flatMap(_.operations.flatMap(buildOperation))
          val serviceShapes = services.flatMap(service => buildService(service, namespaces))
          val model = Model
            .builder()
            .addShapes(traitsModel)
            .addShapes((shapes ++ operations ++ serviceShapes): _*)
            .build()
          validateModel(model)
      }
    }

  private lazy val traitsModel: Model =
    Model.assembler().addImport(TraitsPath.toString).assemble().unwrap()

  def dapHttpTraitsModel: Model = traitsModel
  def dapHttpTraitsPath: Path = TraitsPath

  private def validateModel(model: Model): Either[List[String], Model] = {
    val result = Model.assembler().addModel(model).assemble()
    if (result.isBroken) {
      Left(result.getValidationEvents.asScala.map(_.toString).toList.distinct)
    } else {
      Right(result.unwrap())
    }
  }

  private def serializeModel(model: Model): String = {
    val namespaces = model
      .shapes()
      .iterator()
      .asScala
      .map(_.getId.getNamespace)
      .filter(ns => ns != "smithy.api" && ns != TraitsNamespace)
      .toSet

    val serializer = SmithyIdlModelSerializer
      .builder()
      .shapeFilter(shape => namespaces.contains(shape.getId.getNamespace))
      .build()

    serializer
      .serialize(model)
      .asScala
      .toList
      .sortBy(_._1.toString)
      .map(_._2)
      .mkString("\n\n")
  }

  private def collectShapes(services: List[IrService]): CollectionState =
    services
      .flatMap(_.operations.map(_.output))
      .foldLeft(CollectionState())(visitType)

  private def visitType(state: CollectionState, irType: IrType): CollectionState =
    irType match {
      case struct: IrType.Struct =>
        visitNamedShape(state, struct.id, struct)
      case union: IrType.Union =>
        visitNamedShape(state, union.id, union)
      case listType: IrType.ListType if listType.bytesAlias || listType.bitsAlias =>
        visitType(state, listType.element)
      case listType: IrType.ListType =>
        visitNamedShape(state, listType.id, listType)
      case mapType: IrType.MapType =>
        visitNamedShape(state, mapType.id, mapType)
      case intEnum: IrType.IntEnum =>
        visitNamedShape(state, intEnum.id, intEnum)
      case IrType.Ref(id) if isPreludeShape(id) =>
        state
      case IrType.Ref(id) =>
        if (state.shapeIds.contains(id) || state.visiting.contains(id)) state
        else state.withError(s"Unresolved shape reference '${id.toString}'.")
      case IrType.Primitive(_) =>
        state
      case _: IrType.FunctionPointer =>
        state
    }

  private def visitMemberTarget(state: CollectionState, member: IrMember): CollectionState =
    visitType(state, member.target)

  private def isPreludeShape(id: ShapeId): Boolean =
    id.getNamespace == "smithy.api"

  private def visitNamedShape(
      state: CollectionState,
      id: ShapeId,
      irType: IrType
  ): CollectionState =
    if (state.shapeIds.contains(id)) {
      state
    } else if (state.visiting.contains(id)) {
      state.withShape(id, irType)
    } else {
      val withVisiting = state.withVisiting(id)
      val afterChildren = irType match {
        case struct: IrType.Struct =>
          struct.members.foldLeft(withVisiting)(visitMemberTarget)
        case union: IrType.Union =>
          union.members.foldLeft(withVisiting)(visitMemberTarget)
        case listType: IrType.ListType if listType.bytesAlias || listType.bitsAlias =>
          visitType(withVisiting, listType.element)
        case listType: IrType.ListType =>
          visitType(withVisiting, listType.element)
        case mapType: IrType.MapType =>
          visitType(visitType(withVisiting, mapType.key), mapType.value)
        case _ =>
          withVisiting
      }
      afterChildren.withoutVisiting(id).withShape(id, irType)
    }

  private def buildService(service: IrService, namespaces: Set[String]): Option[ServiceShape] = {
    val namespace = service.operations.headOption.map(_.output.id.getNamespace)
    namespace.flatMap { ns =>
      if (namespaces.contains(ns)) {
        val builder = ServiceShape
          .builder()
          .id(ShapeId.from(s"$ns#${service.name}"))
          .version("1")
          .operations(service.operations.map(op => ShapeId.from(s"$ns#${op.name}")).asJava)

        service.wordSizeBits.foreach(bits => builder.addTrait(intTrait("wordSize", bits)))
        if (service.defaultEndian != IrEndian.Big) {
          builder.addTrait(stringTrait("endian", "little"))
        }

        Some(builder.build())
      } else {
        None
      }
    }
  }

  private def buildOperation(operation: IrOperation): Option[OperationShape] = {
    val namespace = operation.output.id.getNamespace
    val builder = OperationShape
      .builder()
      .id(ShapeId.from(s"$namespace#${operation.name}"))
      .output(operation.output.id)

    builder.addTrait(httpGetTrait(ApiRoutes.normalize(operation.routePath)))

    operation.pointerChain.foreach { chain =>
      builder.addTrait(intTrait("pointerDepth", chain.pointerDepth))
      chain.outerArrayLength.foreach(len => builder.addTrait(intTrait("outerArrayLength", len)))
      if (chain.followCString) builder.addTrait(booleanTrait("followCString", true))
      chain.pointeeType match {
        case struct: IrType.Struct =>
          builder.addTrait(stringTrait("pointeeShape", struct.id.toString))
        case intEnum: IrType.IntEnum =>
          builder.addTrait(stringTrait("pointeeShape", intEnum.id.toString))
        case IrType.Primitive(kind) =>
          builder.addTrait(stringTrait("pointeeShape", preludeShapeId(kind).toString))
        case _ =>
      }
    }

    Some(builder.build())
  }

  private def buildShape(id: ShapeId, irType: IrType): Option[Shape] =
    irType match {
      case struct: IrType.Bitmask =>
        Some(
          buildStructure(
            id,
            struct.members,
            struct.declaredSizeBits,
            isBitmask = true,
            isDapStruct = false
          )
        )
      case struct: IrType.MemoryMappedStruct =>
        Some(
          buildStructure(
            id,
            struct.members,
            struct.declaredSizeBits,
            isBitmask = false,
            isDapStruct = true
          )
        )
      case struct: IrType.EnclosingStruct =>
        Some(
          buildStructure(
            id,
            struct.members,
            struct.declaredSizeBits,
            isBitmask = false,
            isDapStruct = false
          )
        )
      case union: IrType.Union =>
        Some(buildUnion(id, union.members))
      case listType: IrType.ListType if listType.bytesAlias || listType.bitsAlias =>
        None
      case listType: IrType.ListType =>
        Some(
          ListShape
            .builder()
            .id(id)
            .member(targetShapeId(listType.element))
            .build()
        )
      case mapType: IrType.MapType =>
        Some(
          MapShape
            .builder()
            .id(id)
            .key(targetShapeId(mapType.key))
            .value(targetShapeId(mapType.value))
            .build()
        )
      case intEnum: IrType.IntEnum =>
        Some(buildIntEnum(id, intEnum))
      case _ =>
        None
    }

  private def buildIntEnum(id: ShapeId, intEnum: IrType.IntEnum): IntEnumShape = {
    val builder = IntEnumShape.builder().id(id)
    val seenValues = scala.collection.mutable.Set.empty[Int]
    // DESNOTE(jbarber, 2026-07-19): Smithy intEnum member values must be unique, while C allows
    // aliases (A = 1, B = 1). Keep the first name for each value on emit; decode still maps the
    // shared numeric value to that first name via IrType.IntEnum.values order.
    // See https://smithy.io/2.0/spec/simple-types.html#intenum
    intEnum.values.foreach { enumValue =>
      if (seenValues.add(enumValue.value)) {
        val _ = builder.addMember(enumValue.name, enumValue.value)
      }
    }
    builder.build()
  }

  private def buildStructure(
      id: ShapeId,
      members: List[IrMember],
      declaredSizeBits: Option[Int],
      isBitmask: Boolean,
      isDapStruct: Boolean
  ): StructureShape = {
    val builder = StructureShape.builder().id(id)
    if (isBitmask) {
      builder.addTrait(annotationTrait("bitmask"))
    }
    if (isDapStruct) {
      builder.addTrait(annotationTrait("dapStruct"))
    }
    declaredSizeBits.foreach(size => builder.addTrait(intTrait("size", size)))
    members.foreach(member => addMember(builder, member))
    builder.build()
  }

  private def buildUnion(id: ShapeId, members: List[IrMember]): UnionShape = {
    val builder = UnionShape.builder().id(id)
    members.foreach(member => addMember(builder, member))
    builder.build()
  }

  private def addMember(
      builder: StructureShape.Builder,
      member: IrMember
  ): StructureShape.Builder =
    builder.addMember(
      member.name,
      memberTargetShapeId(member),
      memberTraitConsumer(member)
    )

  private def addMember(
      builder: UnionShape.Builder,
      member: IrMember
  ): UnionShape.Builder =
    builder.addMember(
      member.name,
      memberTargetShapeId(member),
      memberTraitConsumer(member)
    )

  private def memberTraitConsumer(member: IrMember): Consumer[MemberShape.Builder] =
    (update: MemberShape.Builder) => memberSmithyTraits(member).foreach(update.addTrait)

  private def memberTargetShapeId(member: IrMember): ShapeId =
    if (member.isPointer && !member.isArray) {
      ShapeId.from("smithy.api#Long")
    } else {
      targetShapeId(member.target)
    }

  private def targetShapeId(irType: IrType): ShapeId =
    irType match {
      case struct: IrType.Struct                            => struct.id
      case union: IrType.Union                              => union.id
      case listType: IrType.ListType if listType.bytesAlias => BytesShapeId
      case listType: IrType.ListType if listType.bitsAlias  => BitsShapeId
      case listType: IrType.ListType                        => listType.id
      case mapType: IrType.MapType                          => mapType.id
      case intEnum: IrType.IntEnum                          => intEnum.id
      case IrType.Ref(id)                                   => id
      case IrType.Primitive(kind)                           => preludeShapeId(kind)
      case _: IrType.FunctionPointer                        => ShapeId.from("smithy.api#Long")
    }

  private def memberSmithyTraits(member: IrMember): List[Trait] =
    List(
      member.staticAddress.map(address => stringTrait("staticAddress", formatAddress(address))),
      member.paddingRepeats.map(repeats => intTrait("padding", repeats)),
      member.readSizeBytes.map(size => intTrait("size", size)),
      Option.when(member.isPointer)(annotationTrait("pointer")),
      Option.when(member.isArray)(annotationTrait("array")),
      member.arrayLength.map(length => intTrait("length", length)),
      member.endianOverride.map {
        case IrEndian.Big    => stringTrait("endian", "big")
        case IrEndian.Little => stringTrait("endian", "little")
      }
    ).flatten ++ memberTraitNames(member).map(annotationTrait) ++
      (member.target match {
        case fp: IrType.FunctionPointer => List(functionPointerTrait(fp))
        case _                          => Nil
      })

  private def memberTraitNames(member: IrMember): List[String] =
    member.primitiveOverride match {
      case Some(kind) =>
        primitiveTraitFor(kind).toList
      case None =>
        member.target match {
          case IrType.Primitive(kind) => unsignedOrCustomTrait(kind).toList
          case _                      => Nil
        }
    }

  private def unsignedOrCustomTrait(kind: IrPrimitive): Option[String] =
    kind match {
      case IrPrimitive.U8 | IrPrimitive.U16 | IrPrimitive.U32 | IrPrimitive.U64 | IrPrimitive.U128 |
          IrPrimitive.F8 | IrPrimitive.F16 | IrPrimitive.Char =>
        primitiveTraitFor(kind)
      case _ =>
        None
    }

  private def primitiveTraitFor(kind: IrPrimitive): Option[String] =
    kind match {
      case IrPrimitive.U8                          => Some("u8")
      case IrPrimitive.S8                          => Some("s8")
      case IrPrimitive.U16                         => Some("u16")
      case IrPrimitive.S16                         => Some("s16")
      case IrPrimitive.U32                         => Some("u32")
      case IrPrimitive.S32                         => Some("s32")
      case IrPrimitive.U64                         => Some("u64")
      case IrPrimitive.S64                         => Some("s64")
      case IrPrimitive.U128                        => Some("u128")
      case IrPrimitive.S128                        => Some("s128")
      case IrPrimitive.F8                          => Some("f8")
      case IrPrimitive.F16                         => Some("f16")
      case IrPrimitive.F32                         => Some("f32")
      case IrPrimitive.F64                         => Some("f64")
      case IrPrimitive.Char                        => Some("char")
      case IrPrimitive.Bool | IrPrimitive.LongWord =>
        None
    }

  private def preludeShapeId(kind: IrPrimitive): ShapeId =
    ShapeId.from(s"smithy.api#${smithyPreludeName(kind)}")

  private def smithyPreludeName(kind: IrPrimitive): String =
    kind match {
      case IrPrimitive.Bool                                                      => "Boolean"
      case IrPrimitive.Char                                                      => "Byte"
      case IrPrimitive.U8 | IrPrimitive.S8                                       => "Byte"
      case IrPrimitive.U16 | IrPrimitive.S16 | IrPrimitive.U32 | IrPrimitive.S32 => "Integer"
      case IrPrimitive.U64 | IrPrimitive.S64 | IrPrimitive.U128 | IrPrimitive.S128 |
          IrPrimitive.LongWord =>
        "Long"
      case IrPrimitive.F8 | IrPrimitive.F16 | IrPrimitive.F32 => "Float"
      case IrPrimitive.F64                                    => "Double"
    }

  private def traitId(name: String): ShapeId =
    ShapeId.from(s"$TraitsNamespace#$name")

  private def annotationTrait(name: String): Trait =
    new DynamicTrait(traitId(name), Node.objectNode())

  private def stringTrait(name: String, value: String): Trait =
    new DynamicTrait(traitId(name), Node.from(value))

  private def intTrait(name: String, value: Int): Trait =
    new DynamicTrait(traitId(name), Node.from(value))

  private def booleanTrait(name: String, value: Boolean): Trait =
    new DynamicTrait(traitId(name), Node.from(value))

  private def functionPointerTrait(fp: IrType.FunctionPointer): Trait = {
    val paramsStr = fp.params.map(p => s"${p.typeName}|${p.name}").mkString(";")
    val node = Node
      .objectNodeBuilder()
      .withMember("name", Node.from(fp.name))
      .withMember("output", Node.from(fp.returnType))
      .withMember("params", Node.from(paramsStr))
      .build()
    new DynamicTrait(traitId("functionPointer"), node)
  }

  private def httpGetTrait(uri: String): Trait = {
    val node = Node
      .objectNodeBuilder()
      .withMember("method", Node.from("GET"))
      .withMember("uri", Node.from(uri))
      .withMember("code", Node.from(200))
      .build()
    new DynamicTrait(ShapeId.from("smithy.api#http"), node)
  }

  private def formatAddress(address: Long): String =
    if (address >= 0) {
      s"0x${address.toHexString}"
    } else {
      s"0x${java.lang.Long.toUnsignedString(address, 16)}"
    }
}
