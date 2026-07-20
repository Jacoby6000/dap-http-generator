package io.github.jacoby6000.daphttp

import scala.collection.mutable.ListBuffer

/** Type-driven C ABI layout for memory-mapped structs.
  *
  * DESNOTE(jbarber, 2026-07-20): Member types + word size are the source of truth for offsets.
  * Header `/* 0xN */` comments are documentation only — validate with [[validateCommentOffsets]],
  * never stamp them onto IR. Natural alignment only (PowerPC-style); `#pragma pack` / packed
  * attributes are not honored yet. See
  * https://refspecs.linuxfoundation.org/elf/ppc-elf-psABI-1.7.pdf
  */
object IrLayout {

  /** Pack members with natural alignment. Returns assigned offsets and aligned sizeof. */
  def packMembers(
      members: List[IrMember],
      wordSize: Option[Int]
  ): Either[List[String], (List[IrMember], Int)] = {
    val errors = ListBuffer.empty[String]
    val groups = groupMembersForLayout(members)
    var offset = 0
    val placed = ListBuffer.empty[IrMember]
    val groupAligns = ListBuffer.empty[Int]

    groups.foreach { group =>
      val sizes = group.map(m => memberSizeBytes(m, wordSize, errors))
      val aligns = group.map(m => memberAlignmentBytes(m, wordSize, errors))
      (sizes.sequence, aligns.sequence) match {
        case (Some(sizeList), Some(alignList)) =>
          val slotSize = sizeList.max
          val slotAlign = alignList.max
          groupAligns += slotAlign
          offset = alignUp(offset, slotAlign)
          group.foreach { member =>
            placed += member.copy(offsetBytes = Some(offset))
          }
          offset += slotSize
        case _ =>
          group.foreach { member =>
            if (!errors.exists(_.startsWith(member.id.toString)))
              errors += s"${member.id}: Unable to determine member size/alignment for packing."
          }
      }
    }

    if (errors.nonEmpty) {
      Left(errors.toList.distinct)
    } else {
      val structAlign = groupAligns.maxOption.getOrElse(1)
      val sizeof = alignUp(offset, structAlign)
      Right((placed.toList, sizeof))
    }
  }

  def packStruct(
      struct: IrType.MemoryMappedStruct,
      wordSize: Option[Int]
  ): Either[List[String], IrType.MemoryMappedStruct] =
    packMembers(struct.members, wordSize).map { case (members, sizeof) =>
      struct.copy(members = members, declaredSizeBits = Some(sizeof))
    }

  /** Compare packed IR offsets to doldecomp-style comment offsets; return warning messages. */
  def validateCommentOffsets(
      structName: String,
      members: List[IrMember],
      commentOffsets: Map[(String, String), Int]
  ): List[String] =
    members.flatMap { member =>
      for {
        packed <- member.offsetBytes
        comment <- commentOffsets.get((structName, member.name))
        if packed != comment
      } yield s"$structName.${member.name}: offset comment 0x${comment.toHexString} disagrees with type-packed layout 0x${packed.toHexString}"
    }

  def groupMembersForLayout(members: List[IrMember]): List[List[IrMember]] = {
    val groups = ListBuffer.empty[List[IrMember]]
    val currentUnion = ListBuffer.empty[IrMember]
    var currentUnionGroup: Option[String] = None

    def flushUnion(): Unit =
      if (currentUnion.nonEmpty) {
        groups += currentUnion.toList
        currentUnion.clear()
        currentUnionGroup = None
      }

    members.foreach { member =>
      member.unionGroup match {
        case Some(group) if currentUnionGroup.contains(group) =>
          currentUnion += member
        case Some(group) =>
          flushUnion()
          currentUnionGroup = Some(group)
          currentUnion += member
        case None =>
          flushUnion()
          groups += List(member)
      }
    }
    flushUnion()
    groups.toList
  }

  def alignUp(value: Int, align: Int): Int =
    if (align <= 1) value
    else (value + align - 1) & ~(align - 1)

  def memberSizeBytes(
      member: IrMember,
      wordSize: Option[Int],
      errors: ListBuffer[String]
  ): Option[Int] = {
    val wordBytes = wordSize.map(_ / 8)
    member.readSizeBytes.orElse {
      member.layoutBitWidth
        .map(bits => math.ceil(bits.toDouble / 8d).toInt)
        .orElse {
          if (member.isPointer && member.isArray) {
            for {
              length <- member.arrayLength.orElse {
                errors += s"${member.id}: Pointer arrays must declare arrayLength."
                None
              }
              wb <- wordBytes.orElse {
                errors += s"${member.id}: Pointer members require word size."
                None
              }
            } yield length * wb
          } else if (member.isPointer) {
            wordBytes.orElse {
              errors += s"${member.id}: Pointer members require word size."
              None
            }
          } else if (member.isArray) {
            member.target match {
              case list: IrType.ListType =>
                for {
                  length <- member.arrayLength.orElse {
                    errors += s"${member.id}: Arrays must declare arrayLength."
                    None
                  }
                  elem <- typeSizeBytes(list.element, wordSize, errors)
                } yield length * elem
              case other =>
                for {
                  length <- member.arrayLength.orElse {
                    errors += s"${member.id}: Arrays must declare arrayLength."
                    None
                  }
                  elem <- typeSizeBytes(other, wordSize, errors)
                } yield length * elem
            }
          } else {
            member.primitiveOverride
              .flatMap(p => typeSizeBytes(IrType.Primitive(p), wordSize, errors))
              .orElse(typeSizeBytes(member.target, wordSize, errors))
          }
        }
    }
  }

  def memberAlignmentBytes(
      member: IrMember,
      wordSize: Option[Int],
      errors: ListBuffer[String]
  ): Option[Int] = {
    val wordBytes = wordSize.map(_ / 8)
    if (member.isPointer) {
      wordBytes.orElse {
        errors += s"${member.id}: Pointer members require word size."
        None
      }
    } else if (member.layoutBitWidth.isDefined) {
      memberSizeBytes(member, wordSize, errors).map(naturalAlignForSize(_, wordSize))
    } else if (member.isArray) {
      member.target match {
        case list: IrType.ListType => typeAlignmentBytes(list.element, wordSize, errors)
        case other                 => typeAlignmentBytes(other, wordSize, errors)
      }
    } else {
      member.primitiveOverride
        .flatMap(p => typeAlignmentBytes(IrType.Primitive(p), wordSize, errors))
        .orElse(typeAlignmentBytes(member.target, wordSize, errors))
    }
  }

  def typeSizeBytes(
      irType: IrType,
      wordSize: Option[Int],
      errors: ListBuffer[String]
  ): Option[Int] =
    irType match {
      case IrType.Primitive(kind) =>
        primitiveSizeBytes(kind, wordSize, errors)
      case intEnum: IrType.IntEnum =>
        primitiveSizeBytes(intEnum.underlying, wordSize, errors)
      case _: IrType.FunctionPointer =>
        wordSize.map(_ / 8).orElse {
          errors += s"$irType: Function pointers require word size."
          None
        }
      case IrType.Ref(_) =>
        // Incomplete / recursive by-value ref: treat as opaque word-sized slot if sized at all.
        wordSize.map(_ / 8).orElse(Some(4))
      case list: IrType.ListType =>
        typeSizeBytes(list.element, wordSize, errors)
      case b: IrType.Bitmask =>
        b.declaredSizeBits
          .map(bits => math.ceil(bits.toDouble / 8d).toInt)
          .orElse {
            errors += s"${b.id}: Bitmask requires declared size."
            None
          }
      case m: IrType.MemoryMappedStruct =>
        m.declaredSizeBits.orElse {
          packMembers(m.members.map(_.copy(offsetBytes = None)), wordSize) match {
            case Right((_, size)) => Some(size)
            case Left(errs)       =>
              errors ++= errs
              None
          }
        }
      case e: IrType.EnclosingStruct =>
        e.declaredSizeBits.orElse {
          packMembers(e.members.map(_.copy(offsetBytes = None)), wordSize) match {
            case Right((_, size)) => Some(size)
            case Left(errs)       =>
              errors ++= errs
              None
          }
        }
      case other =>
        errors += s"$other: Unsupported type for layout size."
        None
    }

  def typeAlignmentBytes(
      irType: IrType,
      wordSize: Option[Int],
      errors: ListBuffer[String]
  ): Option[Int] =
    irType match {
      case IrType.Primitive(kind) =>
        primitiveAlignmentBytes(kind, wordSize)
      case _: IrType.IntEnum =>
        Some(4)
      case _: IrType.FunctionPointer =>
        wordSize.map(_ / 8)
      case IrType.Ref(_) =>
        wordSize.map(_ / 8).orElse(Some(4))
      case list: IrType.ListType =>
        typeAlignmentBytes(list.element, wordSize, errors)
      case b: IrType.Bitmask =>
        typeSizeBytes(b, wordSize, errors).map(naturalAlignForSize(_, wordSize))
      case s: IrType.Struct =>
        val memberAligns =
          s.members.flatMap(m => memberAlignmentBytes(m, wordSize, ListBuffer.empty))
        if (memberAligns.isEmpty) Some(1) else Some(memberAligns.max)
      case _ =>
        Some(1)
    }

  private def primitiveSizeBytes(
      kind: IrPrimitive,
      wordSize: Option[Int],
      errors: ListBuffer[String]
  ): Option[Int] = {
    import IrPrimitive._
    kind match {
      case Bool | U8 | S8 | Char | F8 => Some(1)
      case U16 | S16 | F16            => Some(2)
      case U32 | S32 | F32            => Some(4)
      case U64 | S64 | F64            => Some(8)
      case U128 | S128                => Some(16)
      case LongWord                   =>
        wordSize.map(_ / 8).orElse {
          errors += "LongWord requires word size."
          None
        }
    }
  }

  private def primitiveAlignmentBytes(kind: IrPrimitive, wordSize: Option[Int]): Option[Int] = {
    import IrPrimitive._
    val raw = kind match {
      case U8 | S8 | Char | Bool | F8 => Some(1)
      case U16 | S16 | F16            => Some(2)
      case U32 | S32 | F32            => Some(4)
      case U64 | S64 | F64            => Some(8)
      // DESNOTE(jbarber, 2026-07-20): PowerPC 32-bit EABI caps fundamental alignment at 8;
      // treat u128/s128 like an oversized integer (8-byte align), not 16.
      // See https://refspecs.linuxfoundation.org/elf/ppc-elf-psABI-1.7.pdf
      case U128 | S128 =>
        wordSize match {
          case Some(32) => Some(8)
          case _        => Some(16)
        }
      case LongWord => wordSize.map(_ / 8).orElse(Some(8))
    }
    raw.map(align =>
      wordSize match {
        case Some(32) => math.min(align, 8)
        case _        => align
      }
    )
  }

  private def naturalAlignForSize(sizeBytes: Int, wordSize: Option[Int]): Int = {
    val hardCap = wordSize match {
      case Some(32) => 8
      case Some(n)  => n / 8
      case None     => 16
    }
    val candidates = List(1, 2, 4, 8, 16).filter(a => a <= sizeBytes && a <= hardCap)
    candidates.maxOption.getOrElse(1)
  }

  private implicit class OptionListOps[A](private val opts: List[Option[A]]) extends AnyVal {
    def sequence: Option[List[A]] =
      if (opts.forall(_.isDefined)) Some(opts.flatten) else None
  }
}
