package io.github.jacoby6000.daphttp

import software.amazon.smithy.model.Model
import software.amazon.smithy.model.shapes.{
  ListShape,
  MapShape,
  MemberShape,
  OperationShape,
  ServiceShape,
  Shape,
  ShapeId,
  ShapeType,
  StructureShape,
  UnionShape
}

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.util.Try

object IrExtractor {
  private val DapStructTrait = ShapeId.from("com.jacoby6000.daphttp#dapStruct")
  private val BitmaskTrait = ShapeId.from("com.jacoby6000.daphttp#bitmask")
  private val SizeTrait = ShapeId.from("com.jacoby6000.daphttp#size")
  private val PaddingTrait = ShapeId.from("com.jacoby6000.daphttp#padding")
  private val PointerTrait = ShapeId.from("com.jacoby6000.daphttp#pointer")
  private val ArrayTrait = ShapeId.from("com.jacoby6000.daphttp#array")
  private val LengthTrait = ShapeId.from("com.jacoby6000.daphttp#length")
  private val CStringTrait = ShapeId.from("com.jacoby6000.daphttp#cString")
  private val WordSizeTrait = ShapeId.from("com.jacoby6000.daphttp#wordSize")
  private val StaticAddressTrait = ShapeId.from("com.jacoby6000.daphttp#staticAddress")
  private val U8Trait = ShapeId.from("com.jacoby6000.daphttp#u8")
  private val S8Trait = ShapeId.from("com.jacoby6000.daphttp#s8")
  private val U16Trait = ShapeId.from("com.jacoby6000.daphttp#u16")
  private val S16Trait = ShapeId.from("com.jacoby6000.daphttp#s16")
  private val U32Trait = ShapeId.from("com.jacoby6000.daphttp#u32")
  private val S32Trait = ShapeId.from("com.jacoby6000.daphttp#s32")
  private val BytesShape = ShapeId.from("com.jacoby6000.daphttp#Bytes")
  private val BitsShape = ShapeId.from("com.jacoby6000.daphttp#Bits")

  def buildIrFromModel(model: Model): Either[List[String], List[IrService]] = {
    val errors = ListBuffer.empty[String]
    val services = model.shapes(classOf[ServiceShape]).iterator().asScala.toList

    def buildIrType(shapeId: ShapeId, stack: Set[ShapeId]): IrType = {
      if (stack.contains(shapeId)) {
        IrType.Ref(shapeId)
      } else {
        val shape = model.expectShape(shapeId)
        shape.getType match {
          case ShapeType.STRUCTURE =>
            val structure = shape.asInstanceOf[StructureShape]
            IrType.Struct(
              id = structure.getId,
              members = structure
                .members()
                .asScala
                .toList
                .map(member =>
                  buildIrMember(member, buildIrType(member.getTarget, stack + shapeId))
                ),
              isDapStruct = structure.hasTrait(DapStructTrait),
              isBitmask = structure.hasTrait(BitmaskTrait),
              declaredSizeBits = intTraitValue(structure, SizeTrait)
            )
          case ShapeType.UNION =>
            val union = shape.asInstanceOf[UnionShape]
            IrType.Union(
              id = union.getId,
              members = union
                .members()
                .asScala
                .toList
                .map(member =>
                  buildIrMember(member, buildIrType(member.getTarget, stack + shapeId))
                )
            )
          case ShapeType.LIST =>
            val list = shape.asInstanceOf[ListShape]
            IrType.ListType(
              id = list.getId,
              element = buildIrType(list.getMember.getTarget, stack + shapeId),
              bytesAlias = list.getId == BytesShape,
              bitsAlias = list.getId == BitsShape
            )
          case ShapeType.MAP =>
            val mapShape = shape.asInstanceOf[MapShape]
            IrType.MapType(
              id = mapShape.getId,
              key = buildIrType(mapShape.getKey.getTarget, stack + shapeId),
              value = buildIrType(mapShape.getValue.getTarget, stack + shapeId)
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
      val operations = service.getOperations.asScala.toList.flatMap { operationId =>
        val operation = model.expectShape(operationId, classOf[OperationShape])
        operation.getOutput.toScala.flatMap { outputId =>
          buildIrType(outputId, Set.empty) match {
            case outputStruct: IrType.Struct =>
              val routePath = s"/${service.getId.getName}/${operation.getId.getName}"
              Some(IrOperation(operation.getId.getName, routePath, outputStruct))
            case _ =>
              errors += s"${operation.getId}: Operation output must be a structure."
              None
          }
        }
      }
      IrService(service.getId.getName, wordSize, operations)
    }

    if (errors.nonEmpty) Left(errors.toList.distinct) else Right(irServices)
  }

  private def buildIrMember(member: MemberShape, target: IrType): IrMember =
    IrMember(
      id = member.getId,
      name = member.getMemberName,
      target = target,
      staticAddress = staticAddress(member),
      cStringBytes = cStringBytes(member),
      paddingRepeats = intTraitValue(member, PaddingTrait),
      isPointer = member.hasTrait(PointerTrait),
      isArray = member.hasTrait(ArrayTrait),
      arrayLength = intTraitValue(member, LengthTrait),
      primitiveOverride = memberPrimitiveOverride(member)
    )

  private def primitiveForShapeType(shapeType: ShapeType): Option[IrPrimitive] = {
    shapeType match {
      case ShapeType.BOOLEAN => Some(IrPrimitive.Bool)
      case ShapeType.BYTE    => Some(IrPrimitive.S8)
      case ShapeType.SHORT   => Some(IrPrimitive.S16)
      case ShapeType.INTEGER => Some(IrPrimitive.S32)
      case ShapeType.LONG    => Some(IrPrimitive.LongWord)
      case _                 => None
    }
  }

  private def memberPrimitiveOverride(member: MemberShape): Option[IrPrimitive] = {
    if (member.hasTrait(U8Trait)) {
      Some(IrPrimitive.U8)
    } else if (member.hasTrait(S8Trait)) {
      Some(IrPrimitive.S8)
    } else if (member.hasTrait(U16Trait)) {
      Some(IrPrimitive.U16)
    } else if (member.hasTrait(S16Trait)) {
      Some(IrPrimitive.S16)
    } else if (member.hasTrait(U32Trait)) {
      Some(IrPrimitive.U32)
    } else if (member.hasTrait(S32Trait)) {
      Some(IrPrimitive.S32)
    } else {
      None
    }
  }

  private def cStringBytes(member: MemberShape): Option[Int] = {
    member.findTrait(CStringTrait).toScala.flatMap { rawTrait =>
      val node = rawTrait.toNode
      if (node.isNumberNode) {
        Some(node.expectNumberNode.getValue.intValue())
      } else if (node.isObjectNode) {
        Some(node.expectObjectNode.expectNumberMember("bytes").getValue.intValue())
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
