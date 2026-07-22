package io.github.jacoby6000.daphttp

import io.circe.Json
import io.circe.parser
import io.circe.syntax._
import scodec.Codec
import software.amazon.smithy.model.shapes.ShapeId

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object TypeOverlay {
  def loadDocument(path: Path): Either[String, TypeOverlayDocument] =
    if (!Files.exists(path)) Right(TypeOverlayDocument.empty)
    else {
      val text = new String(Files.readAllBytes(path), StandardCharsets.UTF_8).trim
      if (text.isEmpty) Right(TypeOverlayDocument.empty)
      else
        parser.decode[TypeOverlayDocument](text).left.map(_.getMessage)
    }

  def saveDocument(path: Path, document: TypeOverlayDocument): Unit = {
    Option(path.getParent).foreach { parent =>
      if (!Files.exists(parent)) Files.createDirectories(parent)
    }
    Files.write(path, document.asJson.spaces2.getBytes(StandardCharsets.UTF_8))
    ()
  }

  private val OverlayNamespace = "overlay"

  def isPrimitiveAlias(raw: String): Boolean =
    IrPrimitiveAliases.isKnown(raw)

  def resolveTypeId(
      typeId: String,
      document: TypeOverlayDocument,
      typeIndex: Map[ShapeId, IrType]
  ): Either[String, IrType] = {
    val raw = typeId.trim
    if (raw.isEmpty) Left("typeId must be non-empty.")
    else
      IrPrimitiveAliases.fromAlias(raw) match {
        case Some(primitive) =>
          Right(IrType.Primitive(primitive))
        case None =>
          val shapeId =
            try normalizeShapeId(raw)
            catch {
              case _: IllegalArgumentException =>
                return Left(s"Invalid typeId '$typeId'.")
            }
          structDefFor(document, shapeId) match {
            case Some(_) =>
              Right(IrType.Ref(shapeId))
            case None =>
              typeIndex.get(shapeId) match {
                case Some(found) => Right(found)
                case None        =>
                  uniqueTypeByLeafName(typeIndex, raw) match {
                    case Right(Some(found)) => Right(found)
                    case Right(None)        => Left(s"Unknown typeId '$typeId'.")
                    case Left(err)          => Left(err)
                  }
              }
          }
      }
  }

  def normalizeShapeId(raw: String): ShapeId = {
    val trimmed = raw.trim
    if (trimmed.contains("#")) ShapeId.from(trimmed)
    else ShapeId.from(s"$OverlayNamespace#$trimmed")
  }

  /** Unique IR type for an unqualified leaf name, or an ambiguity error. */
  private[daphttp] def uniqueTypeByLeafName(
      typeIndex: Map[ShapeId, IrType],
      leafName: String
  ): Either[String, Option[IrType]] = {
    val matches = typeIndex.collect {
      case (id, tpe) if id.getName == leafName => id -> tpe
    }.toList
    matches match {
      case Nil          => Right(None)
      case List((_, t)) => Right(Some(t))
      case many         =>
        val ids = many.map(_._1.toString).sorted.mkString(", ")
        Left(s"Type id '$leafName' is ambiguous; matches [$ids]. Use a fully-qualified id.")
    }
  }

  def validate(document: TypeOverlayDocument): Either[List[String], TypeOverlayDocument] = {
    val errors = ListBuffer.empty[String]
    document.structs.foreach { case (id, defn) =>
      if (id.trim.isEmpty) errors += "Overlay struct key must be non-empty."
      validateMembers(s"structs[$id]", defn.members, errors)
    }
    document.newStructs.zipWithIndex.foreach { case (ns, index) =>
      if (ns.id.trim.isEmpty) errors += s"newStructs[$index].id must be non-empty."
      validateMembers(s"newStructs[${ns.id}]", ns.members, errors)
    }
    val newIds =
      document.newStructs
        .map(ns => OverlayDocumentOps.normalizeNewStructId(ns.id))
        .filter(_.nonEmpty)
    if (newIds.distinct.size != newIds.size)
      errors += "newStructs ids must be unique."
    if (errors.nonEmpty) Left(errors.toList) else Right(document)
  }

  private def validateMembers(
      context: String,
      members: List[OverlayMember],
      errors: ListBuffer[String]
  ): Unit = {
    if (members.isEmpty) errors += s"$context: must declare at least one member."
    members.zipWithIndex.foreach { case (member, index) =>
      if (member.name.trim.isEmpty)
        errors += s"$context.members[$index].name must be non-empty."
      if (member.typeId.trim.isEmpty)
        errors += s"$context.members[$index].typeId must be non-empty."
      if (member.isArray && member.arrayLength.forall(_ <= 0))
        errors += s"$context.members[$index]: arrays require a positive arrayLength."
    }
    val names = members.map(_.name.trim).filter(_.nonEmpty)
    if (names.distinct.size != names.size)
      errors += s"$context: member names must be unique."
  }

  def buildTypeIndex(services: List[IrService]): Map[ShapeId, IrType] =
    buildTypeCatalog(services).types

  /** First-seen owning service for each shape id (same visit order as [[buildTypeIndex]]). */
  def buildServiceOwners(services: List[IrService]): Map[ShapeId, IrService] =
    buildTypeCatalog(services).owners

  final case class TypeCatalog(
      types: Map[ShapeId, IrType],
      owners: Map[ShapeId, IrService]
  )

  def buildTypeCatalog(services: List[IrService]): TypeCatalog = {
    val index = mutable.Map.empty[ShapeId, IrType]
    val owners = mutable.Map.empty[ShapeId, IrService]
    def visit(service: IrService, tpe: IrType): Unit =
      tpe match {
        case s: IrType.Struct =>
          if (!index.contains(s.id)) {
            index(s.id) = s
            owners(s.id) = service
            s.members.foreach(m => visit(service, m.target))
          }
        case e: IrType.IntEnum =>
          if (!index.contains(e.id)) {
            index(e.id) = e
            owners(e.id) = service
          }
        case list: IrType.ListType =>
          if (!index.contains(list.id)) {
            index(list.id) = list
            owners(list.id) = service
            visit(service, list.element)
          }
        case union: IrType.Union =>
          if (!index.contains(union.id)) {
            index(union.id) = union
            owners(union.id) = service
            union.members.foreach(m => visit(service, m.target))
          }
        case mapType: IrType.MapType =>
          if (!index.contains(mapType.id)) {
            index(mapType.id) = mapType
            owners(mapType.id) = service
            visit(service, mapType.key)
            visit(service, mapType.value)
          }
        case IrType.Ref(_) =>
          ()
        case _ =>
          ()
      }
    services.foreach { service =>
      service.operations.foreach { op =>
        visit(service, op.output)
        op.pointerChain.foreach(pc => visit(service, pc.pointeeType))
      }
    }
    TypeCatalog(index.toMap, owners.toMap)
  }

  def catalog(
      services: List[IrService],
      document: TypeOverlayDocument,
      includeFields: Boolean = false
  ): List[TypeCatalogEntry] = {
    val index = buildTypeIndex(services)
    val primitives =
      IrPrimitiveAliases.catalogAliases.map(alias => TypeCatalogEntry(alias, "primitive"))
    val fromIndex = index.toList
      .sortBy(_._1.toString)
      .flatMap {
        case (id, s: IrType.Struct) =>
          val fields = s.members.map(catalogField)
          Some(
            TypeCatalogEntry(
              id.toString,
              "struct",
              members = Some(fields.map(_.name)),
              fields = if (includeFields) Some(fields) else None
            )
          )
        case (id, _: IrType.IntEnum) =>
          Some(TypeCatalogEntry(id.toString, "enum"))
        case _ =>
          None
      }
    val fromNew = document.newStructs.map { ns =>
      TypeCatalogEntry(
        normalizeShapeId(ns.id).toString,
        "struct",
        members = Some(ns.members.map(_.name)),
        fields = if (includeFields) Some(ns.members) else None
      )
    }
    primitives ++ fromIndex ++ fromNew
  }

  /** Full field list for one struct (source IR, overlay override, or newStruct). */
  def fieldsFor(
      services: List[IrService],
      document: TypeOverlayDocument,
      typeId: String
  ): Option[List[OverlayMember]] = {
    val trimmed = typeId.trim
    if (trimmed.isEmpty) None
    else {
      // Prefer OverlayDocumentOps so short ids resolve unique `ns#Name` overlay keys the same
      // way as the explorer editor after PUT canonicalize.
      OverlayDocumentOps
        .membersForStruct(document, trimmed)
        .orElse {
          val shapeIdOpt =
            try
              Some(
                if (trimmed.contains("#")) ShapeId.from(trimmed)
                else normalizeShapeId(trimmed)
              )
            catch {
              case _: IllegalArgumentException => None
            }
          shapeIdOpt.flatMap { shapeId =>
            structDefFor(document, shapeId)
              .map(_.members)
              .orElse {
                val index = buildTypeIndex(services)
                resolveIndexedStruct(trimmed, index).map(_.members.map(catalogField))
              }
          }
        }
    }
  }

  def rootTypeKey(irType: IrType): String =
    irType match {
      case s: IrType.Struct       => s.id.toString
      case e: IrType.IntEnum      => e.id.toString
      case list: IrType.ListType  => s"list:${rootTypeKey(list.element)}"
      case IrType.Ref(id)         => id.toString
      case IrType.Primitive(kind) => primitiveAlias(kind)
      case other                  => other.toString
    }

  private def resolveIndexedStruct(
      raw: String,
      index: Map[ShapeId, IrType]
  ): Option[IrType.Struct] = {
    val byFull =
      try index.get(ShapeId.from(raw)).collect { case s: IrType.Struct => s }
      catch { case _: IllegalArgumentException => None }
    byFull
      .orElse {
        if (raw.contains("#")) {
          val normalized = normalizeShapeId(raw)
          index.get(normalized).collect { case s: IrType.Struct => s }
        } else {
          // DESNOTE(jbarber, 2026-07-21): Unqualified names must be unique across namespaces —
          // collectFirst would pick an arbitrary struct (PUT /overlays already rejects this).
          val matches = index.collect {
            case (id, s: IrType.Struct) if id.getName == raw => s
          }.toList
          matches match {
            case List(one) => Some(one)
            case _         => None
          }
        }
      }
  }

  private def catalogField(member: IrMember): OverlayMember = {
    val (typeId, elementIsArray) = typeIdForTarget(member)
    OverlayMember(
      name = member.name,
      typeId = typeId,
      isArray = member.isArray || elementIsArray,
      arrayLength = member.arrayLength,
      isPointer = member.isPointer
    )
  }

  /** Returns (typeId, alreadyCountedAsArrayFromListType). */
  private def typeIdForTarget(member: IrMember): (String, Boolean) = {
    member.primitiveOverride
      .map(p => (primitiveAlias(p), false))
      .getOrElse {
        member.target match {
          case IrType.Primitive(kind) =>
            (primitiveAlias(kind), false)
          case e: IrType.IntEnum =>
            (e.id.toString, false)
          case s: IrType.Struct =>
            (s.id.toString, false)
          case IrType.Ref(id) =>
            (id.toString, false)
          case list: IrType.ListType =>
            val elementId = list.element match {
              case IrType.Primitive(kind) => primitiveAlias(kind)
              case e: IrType.IntEnum      => e.id.toString
              case s: IrType.Struct       => s.id.toString
              case IrType.Ref(id)         => id.toString
              case other                  => other.toString
            }
            (elementId, true)
          case fp: IrType.FunctionPointer =>
            (s"fn:${fp.name}", false)
          case other =>
            (other.toString, false)
        }
      }
  }

  private def primitiveAlias(kind: IrPrimitive): String =
    IrPrimitiveAliases.canonical(kind)

  def affectsDecode(
      irType: IrType,
      document: TypeOverlayDocument,
      typeIndex: Map[ShapeId, IrType]
  ): Boolean =
    document.structs.nonEmpty &&
      collectStructIds(irType, typeIndex).exists(id => document.structs.contains(id.toString))

  def rewriteType(
      irType: IrType,
      document: TypeOverlayDocument,
      typeIndex: Map[ShapeId, IrType],
      wordSize: Option[Int] = None
  ): Either[List[String], IrType] = {
    val errors = ListBuffer.empty[String]
    val rewritten =
      rewriteTypeInner(irType, document, typeIndex, Set.empty, errors, wordSize)
    if (errors.nonEmpty) Left(errors.toList.distinct) else Right(rewritten)
  }

  def compileOverlayCodec(
      irType: IrType,
      document: TypeOverlayDocument,
      typeIndex: Map[ShapeId, IrType],
      endian: IrEndian,
      wordSize: Option[Int]
  ): Either[List[String], (IrType, Codec[Json], Int)] =
    for {
      rewritten <- rewriteType(irType, document, typeIndex, wordSize)
      codec <- HttpRouteIrEmitter.compileCodec(rewritten, endian, wordSize)
      sizeBytes <- HttpRouteIrEmitter.sizeBytesForType(rewritten, wordSize)
    } yield (rewritten, codec, sizeBytes)

  def structDefFor(
      document: TypeOverlayDocument,
      shapeId: ShapeId
  ): Option[OverlayStructDef] =
    document.structs
      .get(shapeId.toString)
      .orElse(document.structs.get(shapeId.getName))
      .orElse {
        document.newStructs
          .find { ns =>
            val nid = normalizeShapeId(ns.id)
            nid == shapeId || ns.id == shapeId.toString || ns.id == shapeId.getName
          }
          .map(ns => OverlayStructDef(ns.members))
      }

  private def collectStructIds(
      irType: IrType,
      typeIndex: Map[ShapeId, IrType],
      seen: Set[ShapeId] = Set.empty
  ): Set[ShapeId] =
    irType match {
      case s: IrType.Struct if seen.contains(s.id) =>
        seen
      case s: IrType.Struct =>
        s.members.foldLeft(seen + s.id) { (acc, member) =>
          collectStructIds(member.target, typeIndex, acc)
        }
      case IrType.Ref(id) if seen.contains(id) =>
        seen
      case IrType.Ref(id) =>
        typeIndex.get(id) match {
          case Some(resolved) => collectStructIds(resolved, typeIndex, seen + id)
          case None           => seen + id
        }
      case list: IrType.ListType =>
        collectStructIds(list.element, typeIndex, seen)
      case _ =>
        seen
    }

  private def rewriteTypeInner(
      irType: IrType,
      document: TypeOverlayDocument,
      typeIndex: Map[ShapeId, IrType],
      resolving: Set[ShapeId],
      errors: ListBuffer[String],
      wordSize: Option[Int]
  ): IrType =
    irType match {
      case IrType.Ref(id) =>
        if (resolving.contains(id)) IrType.Ref(id)
        else
          typeIndex.get(id) match {
            case Some(resolved) =>
              rewriteTypeInner(resolved, document, typeIndex, resolving, errors, wordSize)
            case None =>
              structDefFor(document, id) match {
                case Some(defn) =>
                  buildOverlayStruct(id, defn, document, typeIndex, resolving, errors, wordSize)
                case None =>
                  errors += s"Unresolved type reference: $id"
                  IrType.Ref(id)
              }
          }
      case s: IrType.Struct =>
        if (resolving.contains(s.id)) IrType.Ref(s.id)
        else
          structDefFor(document, s.id) match {
            case Some(defn) =>
              buildOverlayStruct(s.id, defn, document, typeIndex, resolving, errors, wordSize)
            case None =>
              val rewrittenMembers = s.members.map { member =>
                member.copy(
                  target = rewriteTypeInner(
                    member.target,
                    document,
                    typeIndex,
                    resolving + s.id,
                    errors,
                    wordSize
                  )
                )
              }
              copyStruct(s, rewrittenMembers)
          }
      case list: IrType.ListType =>
        list.copy(
          element = rewriteTypeInner(list.element, document, typeIndex, resolving, errors, wordSize)
        )
      case other =>
        other
    }

  private def buildOverlayStruct(
      id: ShapeId,
      defn: OverlayStructDef,
      document: TypeOverlayDocument,
      typeIndex: Map[ShapeId, IrType],
      resolving: Set[ShapeId],
      errors: ListBuffer[String],
      wordSize: Option[Int]
  ): IrType.MemoryMappedStruct = {
    val nextResolving = resolving + id
    val unresolved = defn.members.flatMap { overlayMember =>
      resolveMemberType(overlayMember, document, typeIndex, nextResolving, errors, wordSize).map {
        case (target, primitiveOverride) =>
          val listTarget =
            if (overlayMember.isArray && !overlayMember.isPointer) {
              IrType.ListType(
                id = ShapeId.from(s"${id.getNamespace}#${id.getName}_${overlayMember.name}_list"),
                element = target,
                bytesAlias = false,
                bitsAlias = false
              )
            } else target
          IrMember(
            id = ShapeId.from(s"${id.getNamespace}#${id.getName}_${overlayMember.name}"),
            name = overlayMember.name,
            target = listTarget,
            staticAddress = None,
            paddingRepeats = None,
            isPointer = overlayMember.isPointer,
            isArray = overlayMember.isArray,
            arrayLength = overlayMember.arrayLength,
            endianOverride = None,
            primitiveOverride = primitiveOverride,
            readSizeBytes = None,
            unionGroup = None,
            layoutBitWidth = None,
            offsetBytes = None
          )
      }
    }
    if (unresolved.size != defn.members.size && errors.isEmpty)
      errors += s"$id: failed to resolve one or more overlay members (index may help)."
    // DESNOTE(jbarber, 2026-07-20): Overlay layouts drop source offsets/declared sizes and
    // rebuild via IrLayout (shared with C/Smithy IR). A u8 at 0x18 followed by a pointer still
    // leaves the ABI gap before 0x1C. Removing an explicit pad field still lets a widened prior
    // field absorb those bytes when the new types pack without an alignment gap.
    // See https://refspecs.linuxfoundation.org/elf/ppc-elf-psABI-1.7.pdf
    IrLayout.packMembers(unresolved, wordSize) match {
      case Left(errs) =>
        errors ++= errs
        IrType.MemoryMappedStruct(id = id, members = unresolved, declaredSizeBytes = None)
      case Right((members, sizeof)) =>
        IrType.MemoryMappedStruct(
          id = id,
          members = members,
          declaredSizeBytes = Some(sizeof)
        )
    }
  }

  private def resolveMemberType(
      member: OverlayMember,
      document: TypeOverlayDocument,
      typeIndex: Map[ShapeId, IrType],
      resolving: Set[ShapeId],
      errors: ListBuffer[String],
      wordSize: Option[Int]
  ): Option[(IrType, Option[IrPrimitive])] = {
    val raw = member.typeId.trim
    IrPrimitiveAliases.fromAlias(raw) match {
      case Some(primitive) =>
        Some((IrType.Primitive(primitive), None))
      case None =>
        val shapeIdOpt =
          try Some(normalizeShapeId(raw))
          catch {
            case _: IllegalArgumentException =>
              errors += s"Invalid typeId '${member.typeId}'."
              None
          }
        shapeIdOpt.flatMap { shapeId =>
          if (resolving.contains(shapeId)) {
            Some((IrType.Ref(shapeId), None))
          } else
            structDefFor(document, shapeId) match {
              case Some(defn) =>
                Some(
                  (
                    buildOverlayStruct(
                      shapeId,
                      defn,
                      document,
                      typeIndex,
                      resolving,
                      errors,
                      wordSize
                    ),
                    None
                  )
                )
              case None =>
                typeIndex.get(shapeId) match {
                  case Some(found) =>
                    Some(
                      (
                        rewriteTypeInner(
                          found,
                          document,
                          typeIndex,
                          resolving,
                          errors,
                          wordSize
                        ),
                        None
                      )
                    )
                  case None =>
                    uniqueTypeByLeafName(typeIndex, raw) match {
                      case Right(Some(found)) =>
                        Some(
                          (
                            rewriteTypeInner(
                              found,
                              document,
                              typeIndex,
                              resolving,
                              errors,
                              wordSize
                            ),
                            None
                          )
                        )
                      case Right(None) =>
                        errors += s"Unknown typeId '${member.typeId}'."
                        None
                      case Left(err) =>
                        errors += err
                        None
                    }
                }
            }
        }
    }
  }

  private def copyStruct(struct: IrType.Struct, members: List[IrMember]): IrType.Struct =
    struct match {
      case b: IrType.Bitmask =>
        b.copy(members = members)
      case m: IrType.MemoryMappedStruct =>
        m.copy(members = members)
      case e: IrType.EnclosingStruct =>
        e.copy(members = members)
    }
}
