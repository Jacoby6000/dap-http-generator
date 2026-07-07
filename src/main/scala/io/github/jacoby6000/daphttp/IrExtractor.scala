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
  sealed trait PrimitiveTrait {
    def traitId: ShapeId
    def primitive: IrPrimitive
  }
  object PrimitiveTrait {
    case object U8 extends PrimitiveTrait {
      override val traitId: ShapeId = ShapeId.from("com.jacoby6000.daphttp#u8")
      override val primitive: IrPrimitive = IrPrimitive.U8
    }
    case object S8 extends PrimitiveTrait {
      override val traitId: ShapeId = ShapeId.from("com.jacoby6000.daphttp#s8")
      override val primitive: IrPrimitive = IrPrimitive.S8
    }
    case object U16 extends PrimitiveTrait {
      override val traitId: ShapeId = ShapeId.from("com.jacoby6000.daphttp#u16")
      override val primitive: IrPrimitive = IrPrimitive.U16
    }
    case object S16 extends PrimitiveTrait {
      override val traitId: ShapeId = ShapeId.from("com.jacoby6000.daphttp#s16")
      override val primitive: IrPrimitive = IrPrimitive.S16
    }
    case object U32 extends PrimitiveTrait {
      override val traitId: ShapeId = ShapeId.from("com.jacoby6000.daphttp#u32")
      override val primitive: IrPrimitive = IrPrimitive.U32
    }
    case object S32 extends PrimitiveTrait {
      override val traitId: ShapeId = ShapeId.from("com.jacoby6000.daphttp#s32")
      override val primitive: IrPrimitive = IrPrimitive.S32
    }

    val All: List[PrimitiveTrait] = List(U8, S8, U16, S16, U32, S32)
    val PrimitiveByTraitId: Map[ShapeId, IrPrimitive] = All.map(t => t.traitId -> t.primitive).toMap
  }
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
            val members = structure
              .members()
              .asScala
              .toList
              .map(member =>
                buildIrMember(member, buildIrType(member.getTarget, stack + shapeId))
              )
            val declaredSizeBits = intTraitValue(structure, SizeTrait)
            if (structure.hasTrait(BitmaskTrait)) {
              IrType.Bitmask(
                id = structure.getId,
                members = members,
                declaredSizeBits = declaredSizeBits
              )
            } else if (structure.hasTrait(DapStructTrait)) {
              IrType.MemoryMappedStruct(
                id = structure.getId,
                members = members,
                declaredSizeBits = declaredSizeBits
              )
            } else {
              IrType.EnclosingStruct(
                id = structure.getId,
                members = members,
                declaredSizeBits = declaredSizeBits
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
    member.getAllTraits.keySet.asScala.collectFirst(Function.unlift { traitId =>
      PrimitiveTrait.PrimitiveByTraitId.get(traitId)
    })
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
