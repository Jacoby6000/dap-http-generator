package io.github.jacoby6000.daphttp

import software.amazon.smithy.model.Model
import software.amazon.smithy.model.shapes.IntEnumShape
import software.amazon.smithy.model.shapes.ListShape
import software.amazon.smithy.model.shapes.MapShape
import software.amazon.smithy.model.shapes.MemberShape
import software.amazon.smithy.model.shapes.OperationShape
import software.amazon.smithy.model.shapes.ServiceShape
import software.amazon.smithy.model.shapes.Shape
import software.amazon.smithy.model.shapes.ShapeId
import software.amazon.smithy.model.shapes.ShapeType
import software.amazon.smithy.model.shapes.StructureShape
import software.amazon.smithy.model.shapes.UnionShape

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.util.Try

object SmithyIrGenerator {
  private val DapStructTrait = DapSmithyTraits.DapStruct
  private val BitmaskTrait = DapSmithyTraits.Bitmask
  private val SizeTrait = DapSmithyTraits.Size
  private val PaddingTrait = DapSmithyTraits.Padding
  private val PointerTrait = DapSmithyTraits.Pointer
  private val ArrayTrait = DapSmithyTraits.Array
  private val LengthTrait = DapSmithyTraits.Length
  private val EndianTrait = DapSmithyTraits.Endian
  private val WordSizeTrait = DapSmithyTraits.WordSize
  private val StaticAddressTrait = DapSmithyTraits.StaticAddress
  private val PointerDepthTrait = DapSmithyTraits.PointerDepth
  private val OuterArrayLengthTrait = DapSmithyTraits.OuterArrayLength
  private val FollowCStringTrait = DapSmithyTraits.FollowCString
  private val PointeeShapeTrait = DapSmithyTraits.PointeeShape
  private val FunctionPointerTrait = DapSmithyTraits.FunctionPointer
  private val HttpTrait = DapSmithyTraits.Http
  private val BytesShape = DapSmithyTraits.Bytes
  private val BitsShape = DapSmithyTraits.Bits

  def generateFromModel(model: Model): Either[List[String], List[IrService]] = {
    val errors = ListBuffer.empty[String]
    val services = model.shapes(classOf[ServiceShape]).iterator().asScala.toList

    def buildIrType(shapeId: ShapeId, stack: Set[ShapeId], wordSize: Option[Int]): IrType = {
      if (stack.contains(shapeId)) {
        IrType.Ref(shapeId)
      } else {
        val shape = model.expectShape(shapeId)
        shape.getType match {
          case ShapeType.STRUCTURE =>
            val structure = shape.asInstanceOf[StructureShape]
            val members = structure
              .members()
              .asScala
              .toList
              .map(member =>
                buildIrMember(
                  member,
                  buildIrType(member.getTarget, stack + shapeId, wordSize)
                )
              )
            // DESNOTE(jbarber, 2026-07-21): Smithy `@size` unit depends on the shape kind —
            // bits for `@bitmask`, bytes for `@dapStruct` / enclosing structures.
            val sizeTrait = intTraitValue(structure, SizeTrait)
            if (structure.hasTrait(BitmaskTrait)) {
              IrType.Bitmask(
                id = structure.getId,
                members = members,
                storageBits = sizeTrait
              )
            } else if (structure.hasTrait(DapStructTrait)) {
              val unpacked = IrType.MemoryMappedStruct(
                id = structure.getId,
                members = members,
                declaredSizeBytes = sizeTrait
              )
              // DESNOTE(jbarber, 2026-07-20): Pack from member types so Smithy round-trips
              // (which omit C offset comments) keep ABI layout. Explicit @size on the structure
              // wins over the packed sizeof when present.
              wordSize
                .map(ws => IrLayout.packStruct(unpacked, Some(ws)))
                .getOrElse(Right(unpacked)) match {
                case Right(packed) =>
                  sizeTrait match {
                    case Some(size) => packed.copy(declaredSizeBytes = Some(size))
                    case None       => packed
                  }
                case Left(errs) =>
                  errors ++= errs
                  unpacked
              }
            } else {
              IrType.EnclosingStruct(
                id = structure.getId,
                members = members,
                declaredSizeBytes = sizeTrait
              )
            }
          case ShapeType.UNION =>
            val union = shape.asInstanceOf[UnionShape]
            IrType.Union(
              id = union.getId,
              members = union
                .members()
                .asScala
                .toList
                .map(member =>
                  buildIrMember(
                    member,
                    buildIrType(member.getTarget, stack + shapeId, wordSize)
                  )
                )
            )
          case ShapeType.LIST =>
            val list = shape.asInstanceOf[ListShape]
            IrType.ListType(
              id = list.getId,
              element = buildIrType(list.getMember.getTarget, stack + shapeId, wordSize),
              bytesAlias = list.getId == BytesShape,
              bitsAlias = list.getId == BitsShape
            )
          case ShapeType.MAP =>
            val mapShape = shape.asInstanceOf[MapShape]
            IrType.MapType(
              id = mapShape.getId,
              key = buildIrType(mapShape.getKey.getTarget, stack + shapeId, wordSize),
              value = buildIrType(mapShape.getValue.getTarget, stack + shapeId, wordSize)
            )
          case ShapeType.INT_ENUM =>
            val intEnum = shape.asInstanceOf[IntEnumShape]
            val values = intEnum
              .members()
              .asScala
              .toList
              .flatMap { member =>
                intEnum.getEnumValues.asScala
                  .get(member.getMemberName)
                  .map(value => IrEnumValue(member.getMemberName, value.intValue()))
              }
            IrType.IntEnum(
              id = intEnum.getId,
              values = values,
              underlying = IrPrimitive.S32
            )
          case primitiveType =>
            primitiveForShapeType(primitiveType)
              .map(IrType.Primitive.apply)
              .getOrElse(IrType.Ref(shapeId))
        }
      }
    }

    val irServices = services.map { service =>
      val wordSize = intTraitValue(service, WordSizeTrait)
      if (wordSize.isEmpty) {
        errors += s"${service.getId}: Services must declare @wordSize."
      }
      val defaultEndian = endianValue(service, EndianTrait).getOrElse(IrEndian.Big)
      val operations = service.getOperations.asScala.toList.flatMap { operationId =>
        val operation = model.expectShape(operationId, classOf[OperationShape])
        operation.getOutput.toScala.flatMap { outputId =>
          buildIrType(outputId, Set.empty, wordSize) match {
            case outputStruct: IrType.Struct =>
              val routePath = ApiRoutes.normalize(
                operation
                  .findTrait(HttpTrait)
                  .toScala
                  .flatMap { rawTrait =>
                    val node = rawTrait.toNode
                    if (node.isObjectNode) {
                      node.expectObjectNode.getStringMember("uri").toScala.map(_.getValue)
                    } else None
                  }
                  .getOrElse(s"/${service.getId.getName}/${operation.getId.getName}")
              )
              val pointerChain = intTraitValue(operation, PointerDepthTrait).map { depth =>
                val pointeeType = operation
                  .findTrait(PointeeShapeTrait)
                  .toScala
                  .flatMap { rawTrait =>
                    val node = rawTrait.toNode
                    if (node.isStringNode) {
                      val shapeIdStr = node.expectStringNode.getValue
                      ShapeId.from(shapeIdStr) match {
                        case sid if model.getShape(sid).isPresent =>
                          Some(buildIrType(sid, Set.empty, wordSize))
                        case _ => None
                      }
                    } else None
                  }
                  .getOrElse(IrType.Primitive(IrPrimitive.LongWord))
                IrPointerChain(
                  pointeeType = pointeeType,
                  pointerDepth = depth,
                  outerArrayLength = intTraitValue(operation, OuterArrayLengthTrait),
                  followCString = booleanTraitValue(operation, FollowCStringTrait).getOrElse(false)
                )
              }
              Some(IrOperation(operation.getId.getName, routePath, outputStruct, pointerChain))
            case _ =>
              errors += s"${operation.getId}: Operation output must be a structure."
              None
          }
        }
      }
      IrService(service.getId.getName, wordSize, defaultEndian, operations)
    }

    if (errors.nonEmpty) {
      errors.foreach(error => DapHttpLoggers.irSourceSmithy.warn("{}", error))
      DapHttpLoggers.irSourceSmithy.info(
        "Smithy IR generation failed with {} error(s)",
        Integer.valueOf(errors.distinct.size)
      )
      Left(errors.toList.distinct)
    } else {
      val operationCount = irServices.map(_.operations.size).sum
      DapHttpLoggers.irSourceSmithy.info(
        "Generated IR for {} service(s) and {} operation(s)",
        Integer.valueOf(irServices.size),
        Integer.valueOf(operationCount)
      )
      irServices.foreach { service =>
        DapHttpLoggers.irSourceSmithy.debug(
          "Service {} has {} operation(s)",
          service.name,
          Integer.valueOf(service.operations.size)
        )
      }
      Right(irServices)
    }
  }

  private def buildIrMember(member: MemberShape, target: IrType): IrMember = {
    val resolvedTarget = functionPointerFromTrait(member).getOrElse(target)
    val isFuncPointer = resolvedTarget.isInstanceOf[IrType.FunctionPointer]
    IrMember(
      id = member.getId,
      name = member.getMemberName,
      target = resolvedTarget,
      staticAddress = staticAddress(member),
      paddingRepeats = intTraitValue(member, PaddingTrait),
      isPointer = member.hasTrait(PointerTrait) || isFuncPointer,
      isArray = member.hasTrait(ArrayTrait),
      arrayLength = intTraitValue(member, LengthTrait),
      endianOverride = endianValue(member, EndianTrait),
      primitiveOverride =
        if (isFuncPointer) None
        else memberPrimitiveOverride(member),
      readSizeBytes = intTraitValue(member, SizeTrait)
    )
  }

  private def functionPointerFromTrait(member: MemberShape): Option[IrType.FunctionPointer] = {
    member.findTrait(FunctionPointerTrait).toScala.flatMap { rawTrait =>
      val node = rawTrait.toNode
      if (node.isObjectNode) {
        val obj = node.expectObjectNode
        val name = obj.getStringMember("name").toScala.map(_.getValue)
        val output = obj.getStringMember("output").toScala.map(_.getValue)
        val paramsStr = obj.getStringMember("params").toScala.map(_.getValue)
        val params = paramsStr
          .getOrElse("")
          .split(";")
          .filter(_.nonEmpty)
          .zipWithIndex
          .map { case (entry, idx) =>
            val parts = entry.split("\\|", 2)
            if (parts.length == 2) FunctionPointerParam(parts(0), parts(1))
            else FunctionPointerParam(parts(0), s"arg$idx")
          }
          .toList
        for {
          n <- name
          out <- output
        } yield IrType.FunctionPointer(n, params, out)
      } else {
        None
      }
    }
  }

  private def primitiveForShapeType(shapeType: ShapeType): Option[IrPrimitive] = {
    shapeType match {
      case ShapeType.BOOLEAN => Some(IrPrimitive.Bool)
      case ShapeType.BYTE    => Some(IrPrimitive.S8)
      case ShapeType.SHORT   => Some(IrPrimitive.S16)
      case ShapeType.INTEGER => Some(IrPrimitive.S32)
      case ShapeType.LONG    => Some(IrPrimitive.LongWord)
      case ShapeType.FLOAT   => Some(IrPrimitive.F32)
      case ShapeType.DOUBLE  => Some(IrPrimitive.F64)
      case _                 => None
    }
  }

  private def memberPrimitiveOverride(member: MemberShape): Option[IrPrimitive] = {
    member.getAllTraits.keySet.asScala.collectFirst(Function.unlift { traitId =>
      DapSmithyTraits.PrimitiveByTraitId.get(traitId)
    })
  }

  private def endianValue(shape: Shape, traitId: ShapeId): Option[IrEndian] = {
    shape.findTrait(traitId).toScala.flatMap { rawTrait =>
      val node = rawTrait.toNode
      if (node.isStringNode) {
        node.expectStringNode.getValue.toLowerCase match {
          case "big"    => Some(IrEndian.Big)
          case "little" => Some(IrEndian.Little)
          case _        => None
        }
      } else {
        None
      }
    }
  }

  private def intTraitValue(shape: Shape, traitId: ShapeId): Option[Int] = {
    shape.findTrait(traitId).toScala.flatMap { rawTrait =>
      val node = rawTrait.toNode
      if (node.isNumberNode) {
        Some(node.expectNumberNode.getValue.intValue())
      } else {
        None
      }
    }
  }

  private def booleanTraitValue(shape: Shape, traitId: ShapeId): Option[Boolean] = {
    shape.findTrait(traitId).toScala.flatMap { rawTrait =>
      val node = rawTrait.toNode
      if (node.isBooleanNode) {
        Some(node.expectBooleanNode.getValue)
      } else {
        None
      }
    }
  }

  private def staticAddress(member: MemberShape): Option[Long] = {
    member.findTrait(StaticAddressTrait).toScala.flatMap { rawTrait =>
      val node = rawTrait.toNode
      if (node.isStringNode) {
        val value = node.expectStringNode.getValue.trim
        if (value.startsWith("0x") || value.startsWith("0X")) {
          Try(java.lang.Long.parseUnsignedLong(value.drop(2), 16)).toOption
        } else {
          Try(value.toLong).toOption
        }
      } else if (node.isNumberNode) {
        Some(node.expectNumberNode.getValue.longValue())
      } else {
        None
      }
    }
  }
}
