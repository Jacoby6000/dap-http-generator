package io.github.jacoby6000.daphttp

import software.amazon.smithy.model.shapes.ShapeId

import java.nio.file.Files
import java.nio.file.Path
import scala.collection.mutable

object IrSmithyEmitter {
  private val TraitsNamespace = "com.jacoby6000.daphttp"
  private val BytesShapeName = "Bytes"
  private val BitsShapeName = "Bits"

  def emit(services: List[IrService]): Either[List[String], String] = {
    if (services.isEmpty) {
      Left(List("At least one IR service is required to emit Smithy."))
    } else {
      val errors = mutable.ListBuffer.empty[String]
      val namespace = services
        .flatMap(_.operations.map(_.output.id.getNamespace))
        .headOption
        .getOrElse("generated")
      val collector = new ShapeCollector(services, errors)
      val shapes = collector.collect()
      if (errors.nonEmpty) {
        Left(errors.toList.distinct)
      } else {
        services.foreach(collectServiceTraits(collector.usedTraits))
        Right(renderModel(namespace, services, shapes, collector.usedTraits, collector.usedShapeImports))
      }
    }
  }

  private def collectServiceTraits(usedTraits: mutable.Set[String])(service: IrService): Unit = {
    service.wordSizeBits.foreach(_ => usedTraits += "wordSize")
    if (service.defaultEndian != IrEndian.Big) {
      usedTraits += "endian"
    }
  }

  def emitToPath(services: List[IrService], outputPath: Path): Either[List[String], Unit] =
    emit(services).map { content =>
      val parent = outputPath.getParent
      if (parent != null) {
        Files.createDirectories(parent)
      }
      Files.writeString(outputPath, content)
    }

  private final class ShapeCollector(services: List[IrService], errors: mutable.ListBuffer[String]) {
    private val shapes = mutable.LinkedHashMap.empty[ShapeId, IrType]
    private val visiting = mutable.Set.empty[ShapeId]
    val usedTraits = mutable.Set.empty[String]
    val usedShapeImports = mutable.Set.empty[String]

    def collect(): List[(ShapeId, IrType)] = {
      services.foreach { service =>
        service.operations.foreach { operation =>
          visitType(operation.output)
        }
      }
      shapes.toList
    }

    private def visitType(irType: IrType): Unit =
      irType match {
        case struct: IrType.Struct =>
          visitNamedShape(struct.id, struct)
          struct.members.foreach(member => visitMemberTarget(member))
        case union: IrType.Union =>
          visitNamedShape(union.id, union)
          union.members.foreach(member => visitMemberTarget(member))
        case listType: IrType.ListType if listType.bytesAlias =>
          usedShapeImports += BytesShapeName
          visitType(listType.element)
        case listType: IrType.ListType if listType.bitsAlias =>
          usedShapeImports += BitsShapeName
          visitType(listType.element)
        case listType: IrType.ListType =>
          visitNamedShape(listType.id, listType)
          visitType(listType.element)
        case mapType: IrType.MapType =>
          visitNamedShape(mapType.id, mapType)
          visitType(mapType.key)
          visitType(mapType.value)
        case IrType.Ref(id) =>
          if (!shapes.contains(id)) {
            errors += s"Unresolved shape reference '${id.toString}'."
          }
        case IrType.Primitive(_) =>
          ()
      }

    private def visitMemberTarget(member: IrMember): Unit =
      if (member.isPointer) {
        usedTraits += "pointer"
      }
      if (member.isArray) {
        usedTraits += "array"
      }
      member.staticAddress.foreach(_ => usedTraits += "staticAddress")
      member.paddingRepeats.foreach(_ => usedTraits += "padding")
      member.arrayLength.foreach(_ => usedTraits += "length")
      member.endianOverride.foreach(_ => usedTraits += "endian")
      member.primitiveOverride.foreach(primitiveTraitFor(_).foreach(usedTraits += _))
      visitType(member.target)

    private def visitNamedShape(id: ShapeId, irType: IrType): Unit = {
      if (!shapes.contains(id)) {
        if (visiting.contains(id)) {
          shapes += id -> irType
        } else {
          visiting += id
          irType match {
            case struct: IrType.Struct =>
              struct.members.foreach(member => visitMemberTarget(member))
            case union: IrType.Union =>
              union.members.foreach(member => visitMemberTarget(member))
            case listType: IrType.ListType if listType.bytesAlias || listType.bitsAlias =>
              visitType(listType.element)
            case listType: IrType.ListType =>
              visitType(listType.element)
            case mapType: IrType.MapType =>
              visitType(mapType.key)
              visitType(mapType.value)
            case _ =>
              ()
          }
          shapes += id -> irType
          visiting -= id
        }
      }
    }
  }

  private def renderModel(
      namespace: String,
      services: List[IrService],
      shapes: List[(ShapeId, IrType)],
      usedTraits: mutable.Set[String],
      usedShapeImports: mutable.Set[String]
  ): String = {
    val builder = new StringBuilder
    builder.append("""$version: "2"""")
    builder.append("\n\n")
    builder.append(s"namespace $namespace\n\n")
    renderTraitUses(builder, usedTraits.toSet, usedShapeImports.toSet, shapes)
    shapes.foreach { case (id, irType) =>
      renderShape(builder, id, irType)
      builder.append("\n")
    }
    services.foreach { service =>
      renderService(builder, service)
      builder.append("\n")
    }
    builder.toString
  }

  private def renderTraitUses(
      builder: StringBuilder,
      usedTraits: Set[String],
      usedShapeImports: Set[String],
      shapes: List[(ShapeId, IrType)]
  ): Unit = {
    val traits = mutable.LinkedHashSet.empty[String]
    shapes.foreach { case (_, irType) =>
      irType match {
        case _: IrType.Bitmask =>
          traits += "bitmask"
          traits += "size"
        case _: IrType.MemoryMappedStruct =>
          traits += "dapStruct"
        case struct: IrType.Struct if struct.declaredSizeBits.nonEmpty =>
          traits += "size"
        case _ =>
          ()
      }
    }
    traits ++= usedTraits
    val imports = (traits.toList.sorted ++ usedShapeImports.toList.sorted).distinct
    imports.foreach { name =>
      builder.append(s"use $TraitsNamespace#$name\n")
    }
    if (imports.nonEmpty) {
      builder.append("\n")
    }
  }

  private def renderShape(builder: StringBuilder, id: ShapeId, irType: IrType): Unit =
    irType match {
      case bitmask: IrType.Bitmask =>
        builder.append("@bitmask\n")
        bitmask.declaredSizeBits.foreach(size => builder.append(s"@size($size)\n"))
        builder.append(s"structure ${id.getName} {\n")
        bitmask.members.foreach(renderMember(builder, _))
        builder.append("}\n")
      case struct: IrType.MemoryMappedStruct =>
        builder.append("@dapStruct\n")
        struct.declaredSizeBits.foreach(size => builder.append(s"@size($size)\n"))
        builder.append(s"structure ${id.getName} {\n")
        struct.members.foreach(renderMember(builder, _))
        builder.append("}\n")
      case struct: IrType.EnclosingStruct =>
        struct.declaredSizeBits.foreach(size => builder.append(s"@size($size)\n"))
        builder.append(s"structure ${id.getName} {\n")
        struct.members.foreach(renderMember(builder, _))
        builder.append("}\n")
      case union: IrType.Union =>
        builder.append(s"union ${id.getName} {\n")
        union.members.foreach(renderMember(builder, _))
        builder.append("}\n")
      case listType: IrType.ListType if listType.bytesAlias =>
        builder.append(s"list $BytesShapeName {\n")
        builder.append(s"    member: ${renderTypeReference(listType.element)}\n")
        builder.append("}\n")
      case listType: IrType.ListType if listType.bitsAlias =>
        builder.append(s"list $BitsShapeName {\n")
        builder.append(s"    member: ${renderTypeReference(listType.element)}\n")
        builder.append("}\n")
      case listType: IrType.ListType =>
        builder.append(s"list ${id.getName} {\n")
        builder.append(s"    member: ${renderTypeReference(listType.element)}\n")
        builder.append("}\n")
      case mapType: IrType.MapType =>
        builder.append(s"map ${id.getName} {\n")
        builder.append(s"    key: ${renderTypeReference(mapType.key)}\n")
        builder.append(s"    value: ${renderTypeReference(mapType.value)}\n")
        builder.append("}\n")
      case _ =>
        ()
    }

  private def renderService(builder: StringBuilder, service: IrService): Unit = {
    service.wordSizeBits.foreach(wordSize => builder.append(s"@wordSize($wordSize)\n"))
    if (service.defaultEndian != IrEndian.Big) {
      builder.append("""@endian("little")""")
      builder.append("\n")
    }
    builder.append(s"service ${service.name} {\n")
    builder.append("""    version: "1"""")
    builder.append("\n")
    builder.append(
      s"    operations: [${service.operations.map(op => s"${op.name}").mkString(", ")}]\n"
    )
    builder.append("}\n\n")
    service.operations.foreach { operation =>
      builder.append(s"operation ${operation.name} {\n")
      builder.append(s"    output: ${operation.output.id.getName}\n")
      builder.append("}\n\n")
    }
  }

  private def renderMember(builder: StringBuilder, member: IrMember): Unit = {
    member.staticAddress.foreach { address =>
      builder.append(s"    @staticAddress(\"${formatAddress(address)}\")\n")
    }
    member.paddingRepeats.foreach(repeats => builder.append(s"    @padding($repeats)\n"))
    if (member.isPointer) {
      builder.append("    @pointer\n")
    }
    if (member.isArray) {
      builder.append("    @array\n")
    }
    member.arrayLength.foreach(length => builder.append(s"    @length($length)\n"))
    member.endianOverride.foreach {
      case IrEndian.Big    => builder.append("""    @endian("big")"""")
      case IrEndian.Little => builder.append("""    @endian("little")""")
    }
    effectivePrimitive(member).flatMap(primitiveTraitFor).foreach { traitName =>
      builder.append(s"    @$traitName\n")
    }
    builder.append(s"    ${member.name}: ${renderMemberTargetType(member)}\n")
  }

  private def renderMemberTargetType(member: IrMember): String =
    if (member.isPointer) {
      "Long"
    } else {
      renderTypeReference(member.target)
    }

  private def renderTypeReference(irType: IrType): String =
    irType match {
      case struct: IrType.Struct     => struct.id.getName
      case union: IrType.Union         => union.id.getName
      case listType: IrType.ListType if listType.bytesAlias => BytesShapeName
      case listType: IrType.ListType if listType.bitsAlias  => BitsShapeName
      case listType: IrType.ListType   => listType.id.getName
      case mapType: IrType.MapType     => mapType.id.getName
      case IrType.Ref(id)              => id.getName
      case IrType.Primitive(kind)      => smithyBaseType(kind)
    }

  private def effectivePrimitive(member: IrMember): Option[IrPrimitive] =
    member.primitiveOverride.orElse {
      member.target match {
        case IrType.Primitive(kind) => Some(kind)
        case _                      => None
      }
    }

  private def primitiveTraitFor(kind: IrPrimitive): Option[String] =
    kind match {
      case IrPrimitive.U8   => Some("u8")
      case IrPrimitive.S8   => Some("s8")
      case IrPrimitive.U16  => Some("u16")
      case IrPrimitive.S16  => Some("s16")
      case IrPrimitive.U32  => Some("u32")
      case IrPrimitive.S32  => Some("s32")
      case IrPrimitive.U64  => Some("u64")
      case IrPrimitive.S64  => Some("s64")
      case IrPrimitive.U128 => Some("u128")
      case IrPrimitive.S128 => Some("s128")
      case IrPrimitive.F8   => Some("f8")
      case IrPrimitive.F16  => Some("f16")
      case IrPrimitive.F32  => Some("f32")
      case IrPrimitive.F64  => Some("f64")
      case IrPrimitive.Char => Some("char")
      case IrPrimitive.Bool | IrPrimitive.LongWord =>
        None
    }

  private def smithyBaseType(kind: IrPrimitive): String =
    kind match {
      case IrPrimitive.Bool     => "Boolean"
      case IrPrimitive.Char     => "Byte"
      case IrPrimitive.U8 | IrPrimitive.S8 => "Byte"
      case IrPrimitive.U16 | IrPrimitive.S16 | IrPrimitive.U32 | IrPrimitive.S32 => "Integer"
      case IrPrimitive.U64 | IrPrimitive.S64 | IrPrimitive.U128 | IrPrimitive.S128 | IrPrimitive.LongWord =>
        "Long"
      case IrPrimitive.F8 | IrPrimitive.F16 | IrPrimitive.F32 => "Float"
      case IrPrimitive.F64 => "Double"
    }

  private def formatAddress(address: Long): String =
    if (address >= 0) {
      s"0x${address.toHexString}"
    } else {
      s"0x${java.lang.Long.toUnsignedString(address, 16)}"
    }
}
