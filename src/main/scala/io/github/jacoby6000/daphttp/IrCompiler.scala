package io.github.jacoby6000.daphttp

import scala.collection.mutable.ListBuffer

object IrCompiler {
  def compileRoutePlansFromIr(
      irServices: List[IrService]
  ): Either[List[String], Map[String, RoutePlan]] = {
    val errors = ListBuffer.empty[String]

    irServices.foreach { service =>
      if (service.wordSizeBits.isEmpty) {
        errors += s"${service.name}: Services must declare @wordSize."
      }
    }

    val routePlans = irServices.flatMap { service =>
      service.operations.map { operation =>
        val reads = collectReadsForType(
          operation.output,
          None,
          operation.routePath,
          service.wordSizeBits,
          errors
        )
        operation.routePath -> RoutePlan(operation.routePath, reads)
      }
    }.toMap

    if (errors.nonEmpty) Left(errors.toList.distinct) else Right(routePlans)
  }

  private def collectReadsForType(
      irType: IrType,
      baseAddress: Option[Long],
      pathPrefix: String,
      wordSize: Option[Int],
      errors: ListBuffer[String]
  ): List[ReadPlan] = {
    irType match {
      case struct: IrType.Struct =>
        val isDapShape = struct.isDapStruct || struct.isBitmask
        if (isDapShape) {
          baseAddress match {
            case None =>
              errors += s"${struct.id}: DAP-backed structures must be reachable from @staticAddress members."
              Nil
            case Some(address) =>
              structureSizeBytes(struct, wordSize, errors) match {
                case Some(sizeBytes) =>
                  List(
                    ReadPlan(
                      path = pathPrefix,
                      address = address,
                      sizeBytes = sizeBytes,
                      decodeType = Some(struct),
                      wordSizeBits = wordSize
                    )
                  )
                case None            => Nil
              }
          }
        } else {
          struct.members.flatMap { member =>
            val memberPath = s"$pathPrefix.${member.name}"
            val memberRequiresStaticAddress = member.target match {
              case nestedStruct: IrType.Struct => nestedStruct.isDapStruct || nestedStruct.isBitmask
              case _                           => false
            }
            val memberAddress =
              if (memberRequiresStaticAddress) {
                member.staticAddress.orElse {
                  errors += s"${member.id}: DAP-backed members of non-DAP structures must declare @staticAddress."
                  None
                }
              } else {
                member.staticAddress
              }
            member.target match {
              case nestedStruct: IrType.Struct =>
                collectReadsForType(nestedStruct, memberAddress, memberPath, wordSize, errors)
              case _ =>
                memberAddress
                  .flatMap(address =>
                    memberSizeBytes(member, wordSize, errors).map(sizeBytes =>
                      ReadPlan(
                        path = memberPath,
                        address = address,
                        sizeBytes = sizeBytes,
                        decodeType = Some(memberReadType(member)),
                        wordSizeBits = wordSize
                      )
                    )
                  )
                  .toList
            }
          }
        }
      case union: IrType.Union =>
        errors += s"${union.id}: Union outputs are modeled in IR but not yet readable from static layouts."
        Nil
      case mapType: IrType.MapType =>
        errors += s"${mapType.id}: Map outputs are modeled in IR but not yet readable from static layouts."
        Nil
      case listType: IrType.ListType =>
        errors += s"${listType.id}: Top-level list outputs are modeled in IR but must be wrapped in a structure."
        Nil
      case _: IrType.Primitive =>
        errors += s"$pathPrefix: Primitive outputs are modeled in IR but must be wrapped in a structure."
        Nil
      case ref: IrType.Ref =>
        errors += s"${ref.id}: Unsupported shape for route planning."
        Nil
    }
  }

  private def structureSizeBytes(
      structure: IrType.Struct,
      wordSize: Option[Int],
      errors: ListBuffer[String]
  ): Option[Int] = {
    structure.declaredSizeBits
      .map { raw =>
        if (structure.isBitmask) math.ceil(raw.toDouble / 8d).toInt else raw
      }
      .orElse {
        val bits = structure.members.flatMap(member => memberBitWidth(member, wordSize, errors))
        if (bits.isEmpty) {
          errors += s"${structure.id}: Unable to infer read width; add @size."
          None
        } else {
          Some(math.ceil(bits.sum.toDouble / 8d).toInt)
        }
      }
  }

  private def memberSizeBytes(
      member: IrMember,
      wordSize: Option[Int],
      errors: ListBuffer[String]
  ): Option[Int] = {
    memberBitWidth(member, wordSize, errors).map(bits => math.ceil(bits.toDouble / 8d).toInt)
  }

  private def memberBitWidth(
      member: IrMember,
      wordSize: Option[Int],
      errors: ListBuffer[String]
  ): Option[Int] = {
    if (member.isPointer) {
      return wordSize.orElse {
        errors += s"${member.id}: Pointer members require service @wordSize."
        None
      }
    }

    member.cStringBytes.map(_ * 8).orElse {
      member.primitiveOverride.flatMap(bitsForPrimitive(_, wordSize)).orElse {
        member.target match {
          case IrType.Primitive(kind) =>
            bitsForPrimitive(kind, wordSize)
          case listType: IrType.ListType =>
            listBitWidth(member, listType, wordSize, errors)
          case nestedStruct: IrType.Struct =>
            structureSizeBytes(nestedStruct, wordSize, errors).map(_ * 8)
          case _ =>
            None
        }
      }
    }
  }

  private def listBitWidth(
      member: IrMember,
      listType: IrType.ListType,
      wordSize: Option[Int],
      errors: ListBuffer[String]
  ): Option[Int] = {
    val isPointerArray = member.isArray && member.isPointer
    if (member.isArray && !isPointerArray) {
      member.arrayLength
        .flatMap { length =>
          listElementBitWidth(listType.element, wordSize).map(_ * length)
        }
        .orElse {
          errors += s"${member.id}: Non-pointer arrays must declare @length."
          None
        }
    } else {
      member.paddingRepeats.flatMap { repeats =>
        if (listType.bytesAlias) {
          Some(repeats * 8)
        } else if (listType.bitsAlias) {
          Some(repeats)
        } else {
          errors += s"${member.id}: @padding is only supported for Bytes/Bits list shapes."
          None
        }
      }
    }
  }

  private def listElementBitWidth(elementType: IrType, wordSize: Option[Int]): Option[Int] = {
    elementType match {
      case IrType.Primitive(kind)      => bitsForPrimitive(kind, wordSize)
      case nestedStruct: IrType.Struct =>
        structureSizeBytes(nestedStruct, wordSize, ListBuffer.empty).map(_ * 8)
      case _ =>
        None
    }
  }

  private def bitsForPrimitive(kind: IrPrimitive, wordSize: Option[Int]): Option[Int] = {
    kind match {
      case IrPrimitive.Bool     => Some(1)
      case IrPrimitive.U8       => Some(8)
      case IrPrimitive.S8       => Some(8)
      case IrPrimitive.U16      => Some(16)
      case IrPrimitive.S16      => Some(16)
      case IrPrimitive.U32      => Some(32)
      case IrPrimitive.S32      => Some(32)
      case IrPrimitive.LongWord => wordSize.orElse(Some(64))
    }
  }

  private def memberReadType(member: IrMember): IrType = {
    member.primitiveOverride
      .map(IrType.Primitive.apply)
      .getOrElse(member.target)
  }
}
