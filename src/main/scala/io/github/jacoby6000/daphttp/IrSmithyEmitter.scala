package io.github.jacoby6000.daphttp

import software.amazon.smithy.model.shapes.ShapeId

import java.nio.file.Files
import java.nio.file.Path

object IrSmithyEmitter {
  private val TraitsNamespace = "com.jacoby6000.daphttp"
  private val BytesShapeName = "Bytes"
  private val BitsShapeName = "Bits"

  private final case class CollectionState(
      shapes: List[(ShapeId, IrType)] = Nil,
      usedTraits: Set[String] = Set.empty,
      usedShapeImports: Set[String] = Set.empty,
      errors: List[String] = Nil,
      visiting: Set[ShapeId] = Set.empty
  ) {
    def shapeIds: Set[ShapeId] = shapes.map(_._1).toSet

    def withError(message: String): CollectionState =
      copy(errors = message :: errors)

    def withTraits(traitNames: Set[String]): CollectionState =
      copy(usedTraits = usedTraits ++ traitNames)

    def withShapeImport(name: String): CollectionState =
      copy(usedShapeImports = usedShapeImports + name)

    def withShape(id: ShapeId, irType: IrType): CollectionState =
      if (shapeIds.contains(id)) this else copy(shapes = shapes :+ (id -> irType))

    def withVisiting(id: ShapeId): CollectionState =
      copy(visiting = visiting + id)

    def withoutVisiting(id: ShapeId): CollectionState =
      copy(visiting = visiting - id)
  }

  def emit(services: List[IrService]): Either[List[String], String] =
    if (services.isEmpty) {
      Left(List("At least one IR service is required to emit Smithy."))
    } else {
      val namespace = services
        .flatMap(_.operations.map(_.output.id.getNamespace))
        .headOption
        .getOrElse("generated")
      collectShapes(services) match {
        case state if state.errors.nonEmpty =>
          Left(state.errors.distinct)
        case state =>
          val traits = state.usedTraits ++ serviceTraits(services) ++ shapeTraits(state.shapes)
          Right(renderModel(namespace, services, state.shapes, traits, state.usedShapeImports))
      }
    }

  def emitToPath(services: List[IrService], outputPath: Path): Either[List[String], Unit] =
    emit(services).map { content =>
      val parent = outputPath.getParent
      if (parent != null) {
        Files.createDirectories(parent)
      }
      val _ = Files.writeString(outputPath, content)
      ()
    }

  private def collectShapes(services: List[IrService]): CollectionState =
    services
      .flatMap(_.operations.map(_.output))
      .foldLeft(CollectionState())(visitType)

  private def memberTraits(member: IrMember): Set[String] =
    (Set(
      Option.when(member.isPointer)("pointer"),
      Option.when(member.isArray)("array"),
      member.staticAddress.map(_ => "staticAddress"),
      member.paddingRepeats.map(_ => "padding"),
      member.arrayLength.map(_ => "length"),
      member.endianOverride.map(_ => "endian")
    ).flatten ++ memberTraitNames(member)).toSet

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

  private def serviceTraits(services: List[IrService]): Set[String] =
    services.flatMap { service =>
      List(
        service.wordSizeBits.map(_ => "wordSize"),
        Option.when(service.defaultEndian != IrEndian.Big)("endian")
      ).flatten
    }.toSet

  private def shapeTraits(shapes: List[(ShapeId, IrType)]): Set[String] =
    shapes.flatMap { case (_, irType) =>
      irType match {
        case _: IrType.Bitmask => List("bitmask", "size")
        case struct: IrType.MemoryMappedStruct if struct.declaredSizeBits.nonEmpty =>
          List("dapStruct", "size")
        case _: IrType.MemoryMappedStruct                              => List("dapStruct")
        case struct: IrType.Struct if struct.declaredSizeBits.nonEmpty => List("size")
        case _                                                         => Nil
      }
    }.toSet

  private def visitType(state: CollectionState, irType: IrType): CollectionState =
    irType match {
      case struct: IrType.Struct =>
        visitNamedShape(state, struct.id, struct)
      case union: IrType.Union =>
        visitNamedShape(state, union.id, union)
      case listType: IrType.ListType if listType.bytesAlias =>
        visitType(state.withShapeImport(BytesShapeName), listType.element)
      case listType: IrType.ListType if listType.bitsAlias =>
        visitType(state.withShapeImport(BitsShapeName), listType.element)
      case listType: IrType.ListType =>
        visitNamedShape(state, listType.id, listType)
      case mapType: IrType.MapType =>
        visitNamedShape(state, mapType.id, mapType)
      case IrType.Ref(id) if isPreludeShape(id) =>
        state
      case IrType.Ref(id) =>
        if (state.shapeIds.contains(id)) state
        else state.withError(s"Unresolved shape reference '${id.toString}'.")
      case IrType.Primitive(_) =>
        state
    }

  private def visitMemberTarget(state: CollectionState, member: IrMember): CollectionState =
    visitType(state.withTraits(memberTraits(member)), member.target)

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

  private def renderModel(
      namespace: String,
      services: List[IrService],
      shapes: List[(ShapeId, IrType)],
      usedTraits: Set[String],
      usedShapeImports: Set[String]
  ): String = {
    val sections = List(
      """$version: "2"""",
      "",
      s"namespace $namespace",
      "",
      renderTraitUses(usedTraits, usedShapeImports)
    ) ++ shapes.flatMap { case (id, irType) =>
      renderShape(id, irType) :+ ""
    } ++ services.flatMap { service =>
      renderService(service) :+ ""
    }
    sections.mkString("\n")
  }

  private def renderTraitUses(usedTraits: Set[String], usedShapeImports: Set[String]): String = {
    val imports = (usedTraits.toList.sorted ++ usedShapeImports.toList.sorted).distinct
    if (imports.isEmpty) {
      ""
    } else {
      imports.map(name => s"use $TraitsNamespace#$name").mkString("\n") + "\n"
    }
  }

  private def renderShape(id: ShapeId, irType: IrType): List[String] =
    irType match {
      case bitmask: IrType.Bitmask =>
        List("@bitmask") ++
          bitmask.declaredSizeBits.toList.map(size => s"@size($size)") ++
          List(s"structure ${id.getName} {") ++
          bitmask.members.flatMap(renderMember) ++
          List("}")
      case struct: IrType.MemoryMappedStruct =>
        List("@dapStruct") ++
          struct.declaredSizeBits.toList.map(size => s"@size($size)") ++
          List(s"structure ${id.getName} {") ++
          struct.members.flatMap(renderMember) ++
          List("}")
      case struct: IrType.EnclosingStruct =>
        struct.declaredSizeBits.toList.map(size => s"@size($size)") ++
          List(s"structure ${id.getName} {") ++
          struct.members.flatMap(renderMember) ++
          List("}")
      case union: IrType.Union =>
        List(s"union ${id.getName} {") ++
          union.members.flatMap(renderMember) ++
          List("}")
      case listType: IrType.ListType if listType.bytesAlias =>
        List(
          s"list $BytesShapeName {",
          s"    member: ${renderTypeReference(listType.element)}",
          "}"
        )
      case listType: IrType.ListType if listType.bitsAlias =>
        List(
          s"list $BitsShapeName {",
          s"    member: ${renderTypeReference(listType.element)}",
          "}"
        )
      case listType: IrType.ListType =>
        List(
          s"list ${id.getName} {",
          s"    member: ${renderTypeReference(listType.element)}",
          "}"
        )
      case mapType: IrType.MapType =>
        List(
          s"map ${id.getName} {",
          s"    key: ${renderTypeReference(mapType.key)}",
          s"    value: ${renderTypeReference(mapType.value)}",
          "}"
        )
      case _ =>
        Nil
    }

  private def renderService(service: IrService): List[String] = {
    val header =
      service.wordSizeBits.toList.map(wordSize => s"@wordSize($wordSize)") ++
        Option.when(service.defaultEndian != IrEndian.Big)("""@endian("little")""").toList ++
        List(
          s"service ${service.name} {",
          """    version: "1"""",
          s"    operations: [${service.operations.map(_.name).mkString(", ")}]",
          "}"
        )
    val operations = service.operations.flatMap { operation =>
      List(
        s"operation ${operation.name} {",
        s"    output: ${operation.output.id.getName}",
        "}"
      ) :+ ""
    }
    header ++ List("") ++ operations
  }

  private def renderMember(member: IrMember): List[String] = {
    val annotations =
      member.staticAddress.toList.map(address =>
        s"""    @staticAddress("${formatAddress(address)}")"""
      ) ++
        member.paddingRepeats.toList.map(repeats => s"    @padding($repeats)") ++
        Option.when(member.isPointer)("    @pointer").toList ++
        Option.when(member.isArray)("    @array").toList ++
        member.arrayLength.toList.map(length => s"    @length($length)") ++
        member.endianOverride.toList.map {
          case IrEndian.Big    => """    @endian("big")"""
          case IrEndian.Little => """    @endian("little")"""
        } ++
        memberTraitNames(member).map(traitName => s"    @$traitName")
    annotations :+ s"    ${member.name}: ${renderMemberTargetType(member)}"
  }

  private def renderMemberTargetType(member: IrMember): String =
    if (member.isPointer) {
      "Long"
    } else {
      renderTypeReference(member.target)
    }

  private def renderTypeReference(irType: IrType): String =
    irType match {
      case struct: IrType.Struct                            => struct.id.getName
      case union: IrType.Union                              => union.id.getName
      case listType: IrType.ListType if listType.bytesAlias => BytesShapeName
      case listType: IrType.ListType if listType.bitsAlias  => BitsShapeName
      case listType: IrType.ListType                        => listType.id.getName
      case mapType: IrType.MapType                          => mapType.id.getName
      case IrType.Ref(id)                                   => id.getName
      case IrType.Primitive(kind)                           => smithyBaseType(kind)
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

  private def smithyBaseType(kind: IrPrimitive): String =
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

  private def formatAddress(address: Long): String =
    if (address >= 0) {
      s"0x${address.toHexString}"
    } else {
      s"0x${java.lang.Long.toUnsignedString(address, 16)}"
    }
}
