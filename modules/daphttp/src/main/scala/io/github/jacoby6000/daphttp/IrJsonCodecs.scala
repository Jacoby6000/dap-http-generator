package io.github.jacoby6000.daphttp

import io.circe.Json
import scodec.Attempt
import scodec.Codec
import scodec.DecodeResult
import scodec.Err
import scodec.SizeBound
import scodec.bits.BitVector
import scodec.codecs.bits

import java.nio.charset.StandardCharsets
import scala.collection.mutable.ListBuffer

/** IR → scodec JSON codecs, sizing bridges, and decoded-address annotation. */
object IrJsonCodecs {
  def compileCodec(
      irType: IrType,
      endian: IrEndian,
      wordSize: Option[Int]
  ): Either[List[String], Codec[Json]] = {
    val errors = ListBuffer.empty[String]
    compileJsonCodecForType(irType, endian, wordSize, errors, irType.toString) match {
      case Some(codec) if errors.isEmpty => Right(codec)
      case Some(_)                       => Left(errors.toList.distinct)
      case None                          =>
        Left(
          if (errors.isEmpty) List(s"Unable to compile codec for $irType")
          else errors.toList.distinct
        )
    }
  }

  /** Packed / declared size in bytes for an IR type (used by type overlays). */
  def sizeBytesForType(
      irType: IrType,
      wordSize: Option[Int]
  ): Either[List[String], Int] = {
    val errors = ListBuffer.empty[String]
    val size = irType match {
      case struct: IrType.Struct =>
        structureSizeBytes(struct, wordSize, errors)
      case other =>
        pointeeSizeBytes(other, wordSize, errors)
    }
    size match {
      case Some(n) if errors.isEmpty => Right(n)
      case Some(_)                   => Left(errors.toList.distinct)
      case None                      =>
        Left(
          if (errors.isEmpty) List(s"Unable to determine size for $irType")
          else errors.toList.distinct
        )
    }
  }

  /** Compile a JSON memory codec for nested member-path reads/watches. */
  def codecForType(
      irType: IrType,
      endian: IrEndian,
      wordSize: Option[Int]
  ): Option[Codec[Json]] = {
    val errors = ListBuffer.empty[String]
    compileJsonCodecForType(irType, endian, wordSize, errors, "member-path")
  }

  /** Inject `_address` hex fields into every decoded struct (and struct array element). */
  def annotateDecodedAddresses(
      irType: IrType,
      decoded: Json,
      baseAddress: Long,
      wordSize: Option[Int],
      elementStrideBytes: Option[Int] = None
  ): Json =
    irType match {
      case struct: IrType.Struct =>
        annotateStructAddresses(struct, decoded, baseAddress, wordSize)
      case listType: IrType.ListType =>
        annotateArrayAddresses(listType, decoded, baseAddress, wordSize, elementStrideBytes)
      case _ =>
        decoded
    }

  private[daphttp] def annotateStructAddresses(
      struct: IrType.Struct,
      decoded: Json,
      baseAddress: Long,
      wordSize: Option[Int]
  ): Json =
    decoded.asObject match {
      case None =>
        decoded
      case Some(obj) =>
        val errors = ListBuffer.empty[String]
        val offsets = computeMemberOffsets(struct, wordSize, errors)
        val withoutPrior =
          obj.filterKeys(k => k != "_address" && k != "_offsets" && k != "_pointer")
        val wasPointer = obj("_pointer").flatMap(_.asBoolean).contains(true)
        val annotatedMembers = struct.members.foldLeft(withoutPrior) { (acc, member) =>
          acc(member.name) match {
            case None =>
              acc
            case Some(fieldJson) =>
              val fieldAddress = baseAddress + offsets.getOrElse(member.name, 0).toLong
              acc.add(
                member.name,
                annotateMemberAddresses(member, fieldJson, fieldAddress, wordSize)
              )
          }
        }
        // DESNOTE(jbarber, 2026-07-20): `_offsets` lets the dual decode UI align source vs
        // overlay fields by byte offset (renames share a row; missing sides leave gaps).
        // `_pointer` marks values that were followed from a pointer slot so the UI can focus
        // the pointee as a root tab.
        val offsetFields =
          struct.members.flatMap { member =>
            if (!annotatedMembers.contains(member.name)) None
            else
              offsets.get(member.name).map { off =>
                member.name -> Json.fromInt(off)
              }
          }
        val meta =
          List(
            "_address" -> Json.fromString(formatHexAddress(baseAddress)),
            "_offsets" -> Json.obj(offsetFields: _*)
          ) ++ (if (wasPointer) List("_pointer" -> Json.True) else Nil)
        Json.obj((meta ++ annotatedMembers.toList): _*)
    }

  private[daphttp] def annotateMemberAddresses(
      member: IrMember,
      fieldJson: Json,
      fieldAddress: Long,
      wordSize: Option[Int]
  ): Json =
    if (member.isPointer) {
      fieldJson
    } else {
      member.target match {
        case nested: IrType.Struct =>
          annotateStructAddresses(nested, fieldJson, fieldAddress, wordSize)
        case listType: IrType.ListType if member.isArray =>
          annotateArrayAddresses(
            listType,
            fieldJson,
            fieldAddress,
            wordSize,
            arrayElementStrideBytes(member, wordSize)
          )
        case _ =>
          fieldJson
      }
    }

  private[daphttp] def annotateArrayAddresses(
      listType: IrType.ListType,
      decoded: Json,
      baseAddress: Long,
      wordSize: Option[Int],
      elementStrideBytes: Option[Int]
  ): Json =
    listType.element match {
      case nested: IrType.Struct =>
        decoded.asArray match {
          case None =>
            decoded
          case Some(elements) =>
            val errors = ListBuffer.empty[String]
            val layoutSize =
              structureSizeBytes(nested, wordSize, errors).getOrElse(0).toLong
            val elementSize = elementStrideBytes.map(_.toLong).getOrElse(layoutSize)
            Json.arr(
              elements.zipWithIndex.map { case (element, index) =>
                annotateStructAddresses(
                  nested,
                  element,
                  baseAddress + index.toLong * elementSize,
                  wordSize
                )
              }: _*
            )
        }
      case _ =>
        decoded
    }

  private[daphttp] def formatHexAddress(address: Long): String =
    f"0x$address%x"

  private[daphttp] def computeMemberOffsets(
      struct: IrType.Struct,
      wordSize: Option[Int],
      errors: ListBuffer[String]
  ): Map[String, Int] = {
    val packed = ensurePackedOffsets(struct, wordSize, errors)
    packed.members.flatMap(m => m.offsetBytes.map(m.name -> _)).toMap
  }

  private[daphttp] def pointeeSizeBytes(
      pointeeType: IrType,
      wordSize: Option[Int],
      errors: ListBuffer[String]
  ): Option[Int] =
    pointeeType match {
      case struct: IrType.Struct =>
        structureSizeBytes(struct, wordSize, errors)
      case intEnum: IrType.IntEnum =>
        IrLayout
          .bitsForPrimitive(intEnum.underlying, wordSize)
          .map(bits => math.ceil(bits.toDouble / 8d).toInt)
      case IrType.Primitive(kind) =>
        IrLayout.bitsForPrimitive(kind, wordSize).map(bits => math.ceil(bits.toDouble / 8d).toInt)
      case _: IrType.FunctionPointer =>
        wordSize.map(_ / 8)
      case _ =>
        errors += s"$pointeeType: Unsupported pointer chain pointee type."
        None
    }

  private[daphttp] def structureSizeBytes(
      structure: IrType.Struct,
      wordSize: Option[Int],
      errors: ListBuffer[String]
  ): Option[Int] = {
    val packed = ensurePackedOffsets(structure, wordSize, errors)
    (packed match {
      case b: IrType.Bitmask => b.storageBits.map(bits => math.ceil(bits.toDouble / 8d).toInt)
      case m: IrType.MemoryMappedStruct => m.declaredSizeBytes
      case e: IrType.EnclosingStruct    => e.declaredSizeBytes
    }).orElse {
      if (packed.members.exists(_.offsetBytes.isDefined)) {
        packed.members.flatMap { member =>
          for {
            offset <- member.offsetBytes
            sizeBytes <- memberSizeBytes(member, wordSize, errors)
          } yield offset + sizeBytes
        }.maxOption
      } else {
        val bits = packed.members.flatMap(member => memberBitWidth(member, wordSize, errors))
        if (bits.isEmpty) {
          errors += s"${packed.id}: Unable to infer read width; add @size."
          None
        } else {
          Some(math.ceil(bits.sum.toDouble / 8d).toInt)
        }
      }
    }
  }

  private[daphttp] def ensurePackedOffsets(
      structure: IrType.Struct,
      wordSize: Option[Int],
      errors: ListBuffer[String]
  ): IrType.Struct =
    IrLayout.ensureMemberOffsets(structure, wordSize, errors)

  private[daphttp] def memberSizeBytes(
      member: IrMember,
      wordSize: Option[Int],
      errors: ListBuffer[String]
  ): Option[Int] =
    IrLayout.memberSizeBytes(member, wordSize, errors)

  private[daphttp] def memberBitWidth(
      member: IrMember,
      wordSize: Option[Int],
      errors: ListBuffer[String]
  ): Option[Int] = {
    member.layoutBitWidth.orElse {
      if (member.isPointer && member.isArray) {
        member.arrayLength
          .flatMap { length =>
            IrLayout.bitsForPrimitive(IrPrimitive.LongWord, wordSize).map(_ * length)
          }
          .orElse {
            errors += s"${member.id}: Pointer arrays must declare @length."
            None
          }
      } else if (member.isArray && member.target.isInstanceOf[IrType.ListType]) {
        listBitWidth(member, member.target.asInstanceOf[IrType.ListType], wordSize, errors)
      } else if (member.isPointer) {
        wordSize.orElse {
          errors += s"${member.id}: Pointer members require service @wordSize."
          None
        }
      } else {
        member.primitiveOverride.flatMap(IrLayout.bitsForPrimitive(_, wordSize)).orElse {
          member.target match {
            case IrType.Primitive(kind) =>
              IrLayout.bitsForPrimitive(kind, wordSize)
            case intEnum: IrType.IntEnum =>
              IrLayout.bitsForPrimitive(intEnum.underlying, wordSize)
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
  }

  private[daphttp] def isFloatingPrimitive(kind: IrPrimitive): Boolean = {
    kind match {
      case IrPrimitive.F8 | IrPrimitive.F16 | IrPrimitive.F32 | IrPrimitive.F64 => true
      case _                                                                    => false
    }
  }

  private[daphttp] def isSignedPrimitive(kind: IrPrimitive): Boolean = {
    kind match {
      case IrPrimitive.S8 | IrPrimitive.S16 | IrPrimitive.S32 | IrPrimitive.S64 | IrPrimitive.S128 |
          IrPrimitive.LongWord =>
        true
      case _ => false
    }
  }

  private[daphttp] def needsEndian(kind: IrPrimitive): Boolean = {
    kind match {
      case IrPrimitive.Bool | IrPrimitive.Char | IrPrimitive.F8 => false
      case _                                                    => true
    }
  }

  private[daphttp] def applyEndianToBits(
      value: BitVector,
      bitWidth: Int,
      endian: IrEndian
  ): BitVector = {
    if (bitWidth % 8 != 0 || bitWidth <= 8 || endian == IrEndian.Big) {
      value
    } else {
      val byteCount = bitWidth / 8
      val bytes = value.take(bitWidth.toLong).toByteArray
      val padded =
        if (bytes.length == byteCount) bytes
        else Array.fill[Byte](byteCount - bytes.length)(0) ++ bytes
      BitVector(padded.reverse)
    }
  }

  private[daphttp] def parseF16(raw: Long): Double = {
    val sign = if ((raw & 0x8000L) == 0L) 1.0 else -1.0
    val exponent = ((raw >> 10) & 0x1fL).toInt
    val fraction = (raw & 0x03ffL).toInt
    if (exponent == 0) {
      if (fraction == 0) sign * 0.0 else sign * math.pow(2.0, -14.0) * (fraction.toDouble / 1024.0)
    } else if (exponent == 31) {
      if (fraction == 0) sign * Double.PositiveInfinity else Double.NaN
    } else {
      sign * math.pow(2.0, exponent - 15.0) * (1.0 + fraction.toDouble / 1024.0)
    }
  }

  private[daphttp] def parseF8(raw: Long): Double = {
    val sign = if ((raw & 0x80L) == 0L) 1.0 else -1.0
    val exponent = ((raw >> 3) & 0x0fL).toInt
    val fraction = (raw & 0x07L).toInt
    if (exponent == 0) {
      if (fraction == 0) sign * 0.0 else sign * math.pow(2.0, -6.0) * (fraction.toDouble / 8.0)
    } else if (exponent == 15) {
      if (fraction == 0) sign * Double.PositiveInfinity else Double.NaN
    } else {
      sign * math.pow(2.0, exponent - 7.0) * (1.0 + fraction.toDouble / 8.0)
    }
  }

  private[daphttp] def floatingJson(kind: IrPrimitive, raw: BigInt): Json = {
    kind match {
      case IrPrimitive.F8 =>
        Json.fromDoubleOrNull(parseF8(raw.longValue))
      case IrPrimitive.F16 =>
        Json.fromDoubleOrNull(parseF16(raw.longValue))
      case IrPrimitive.F32 =>
        Json.fromFloatOrNull(java.lang.Float.intBitsToFloat((raw.longValue & 0xffffffffL).toInt))
      case IrPrimitive.F64 =>
        Json.fromDoubleOrNull(java.lang.Double.longBitsToDouble(raw.longValue))
      case _ =>
        Json.Null
    }
  }

  private[daphttp] def signedJson(bitWidth: Int, raw: BigInt): Json = {
    val value = signExtend(raw, bitWidth)
    if (bitWidth > 64) Json.fromString(value.toString) else Json.fromLong(value.longValue)
  }

  private[daphttp] def unsignedJson(bitWidth: Int, raw: BigInt): Json = {
    if (bitWidth > 63) {
      Json.fromString(raw.toString)
    } else {
      Json.fromLong(raw.longValue)
    }
  }

  private[daphttp] def charJson(raw: BigInt): Json = {
    val c = (raw.longValue & 0xffL).toChar
    Json.fromString(c.toString)
  }

  private[daphttp] def isCharStringArray(member: IrMember, listType: IrType.ListType): Boolean =
    member.isArray && !member.isPointer && listType.element == IrType.Primitive(IrPrimitive.Char)

  private[daphttp] def isCharType(member: IrMember): Boolean =
    memberReadType(member) == IrType.Primitive(IrPrimitive.Char) ||
      member.primitiveOverride.contains(IrPrimitive.Char)

  private[daphttp] def inlineCharByteCount(member: IrMember): Option[Int] =
    if (!isCharType(member) || member.isPointer) {
      None
    } else {
      member.readSizeBytes.orElse {
        if (member.isArray) member.arrayLength else None
      }
    }

  private[daphttp] def nullTerminatedAsciiString(bytes: Array[Byte]): String = {
    val end = bytes.indexWhere(_ == 0)
    val slice = if (end >= 0) bytes.take(end) else bytes
    new String(slice, StandardCharsets.US_ASCII)
  }

  private[daphttp] def bytesFromBitVector(input: BitVector, byteCount: Int): Array[Byte] =
    (0 until byteCount).map { index =>
      bitVectorToUnsigned(input.slice(index * 8L, (index + 1) * 8L)).toByte
    }.toArray

  private[daphttp] def inlineCharArrayStringCodec(count: Int): Codec[Json] =
    new Codec[Json] {
      private val totalBits = count.toLong * 8L

      override def sizeBound: SizeBound = SizeBound.exact(totalBits)

      override def encode(value: Json): Attempt[BitVector] =
        Attempt.failure(Err("Encoding is not supported for read-only DAP proxy codecs."))

      override def decode(input: BitVector): Attempt[DecodeResult[Json]] =
        if (input.size < totalBits) {
          Attempt.failure(
            Err(
              s"Insufficient bits for char array decode. Needed $totalBits bits, got ${input.size} bits."
            )
          )
        } else {
          val bytes = bytesFromBitVector(input, count)
          Attempt.successful(
            DecodeResult(
              Json.fromString(nullTerminatedAsciiString(bytes)),
              input.drop(totalBits)
            )
          )
        }
    }

  private[daphttp] def primitiveJson(kind: IrPrimitive, bitWidth: Int, raw: BigInt): Json = {
    kind match {
      case IrPrimitive.Bool                          => Json.fromBoolean(raw != 0L)
      case IrPrimitive.Char                          => charJson(raw)
      case floating if isFloatingPrimitive(floating) =>
        floatingJson(floating, raw)
      case signed if isSignedPrimitive(signed) =>
        signedJson(bitWidth, raw)
      case _ =>
        unsignedJson(bitWidth, raw)
    }
  }

  private[daphttp] def primitiveToBitVector(bitWidth: Int): BitVector = {
    BitVector.low(bitWidth.toLong)
  }

  private[daphttp] def primitiveCodec(
      kind: IrPrimitive,
      bitWidth: Int,
      endian: IrEndian
  ): Codec[Json] =
    bits(bitWidth.toLong).xmap[Json](
      value => {
        val normalized =
          if (needsEndian(kind)) applyEndianToBits(value, bitWidth, endian) else value
        val raw = bitVectorToUnsigned(normalized)
        primitiveJson(kind, bitWidth, raw)
      },
      _ => primitiveToBitVector(bitWidth)
    )

  private[daphttp] def listBitWidth(
      member: IrMember,
      listType: IrType.ListType,
      wordSize: Option[Int],
      errors: ListBuffer[String]
  ): Option[Int] = {
    val isPointerArray = member.isArray && member.isPointer
    if (isPointerArray) {
      member.arrayLength
        .flatMap { length =>
          IrLayout.bitsForPrimitive(IrPrimitive.LongWord, wordSize).map(_ * length)
        }
        .orElse {
          errors += s"${member.id}: Pointer arrays must declare @length."
          None
        }
    } else if (member.isArray && !isPointerArray) {
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

  private[daphttp] def listElementBitWidth(
      elementType: IrType,
      wordSize: Option[Int]
  ): Option[Int] = {
    elementType match {
      case IrType.Primitive(kind)      => IrLayout.bitsForPrimitive(kind, wordSize)
      case intEnum: IrType.IntEnum     => IrLayout.bitsForPrimitive(intEnum.underlying, wordSize)
      case nestedStruct: IrType.Struct =>
        structureSizeBytes(nestedStruct, wordSize, ListBuffer.empty).map(_ * 8)
      case _ =>
        None
    }
  }

  private[daphttp] def memberReadType(member: IrMember): IrType = {
    member.primitiveOverride
      .map(IrType.Primitive.apply)
      .getOrElse(member.target)
  }

  private[daphttp] def compileJsonCodec(
      irType: Option[IrType],
      endian: IrEndian,
      wordSize: Option[Int],
      errors: ListBuffer[String],
      context: String
  ): Option[Codec[Json]] = {
    irType.flatMap { tpe =>
      compileJsonCodecForType(tpe, endian, wordSize, errors, context)
    }
  }

  private[daphttp] def compileJsonCodecForType(
      irType: IrType,
      endian: IrEndian,
      wordSize: Option[Int],
      errors: ListBuffer[String],
      context: String
  ): Option[Codec[Json]] = {
    irType match {
      case struct: IrType.Struct =>
        compileStructCodec(struct, endian, wordSize, errors, context)
      case intEnum: IrType.IntEnum =>
        compileIntEnumCodec(intEnum, endian, wordSize)
      case IrType.Primitive(kind) =>
        compilePrimitiveCodec(kind, endian, wordSize)
      case fp: IrType.FunctionPointer =>
        functionPointerCodec(fp, endian, wordSize)
      case _ =>
        errors += s"$context: Unable to derive decode codec for output type."
        None
    }
  }

  private[daphttp] final case class CompiledMemberCodec(
      name: String,
      bitWidth: Int,
      codec: Codec[Json]
  )

  private[daphttp] sealed trait LayoutSlot {
    def bitWidth: Int
  }
  private[daphttp] final case class NormalLayoutSlot(compiled: CompiledMemberCodec)
      extends LayoutSlot {
    override def bitWidth: Int = compiled.bitWidth
  }
  private[daphttp] final case class UnionLayoutSlot(
      groupName: String,
      members: List[CompiledMemberCodec]
  ) extends LayoutSlot {
    override def bitWidth: Int = members.map(_.bitWidth).max
  }

  private[daphttp] final case class PaddingLayoutSlot(paddingBits: Int) extends LayoutSlot {
    override def bitWidth: Int = paddingBits
  }

  private[daphttp] def compileStructCodec(
      struct: IrType.Struct,
      endian: IrEndian,
      wordSize: Option[Int],
      errors: ListBuffer[String],
      context: String
  ): Option[Codec[Json]] = {
    val layoutStruct = ensurePackedOffsets(struct, wordSize, errors)
    val layoutGroups = IrLayout.groupMembersForLayout(layoutStruct.members)
    var currentOffsetBits = 0
    val compiledSlots = layoutGroups.flatMap { group =>
      val groupOffsetBits = group.flatMap(_.offsetBytes).headOption.map(_ * 8)
      val paddingSlots = groupOffsetBits.toList.flatMap { offsetBits =>
        if (offsetBits > currentOffsetBits) {
          val padding = offsetBits - currentOffsetBits
          currentOffsetBits = offsetBits
          Some(PaddingLayoutSlot(padding))
        } else {
          None
        }
      }
      val memberSlot = if (group.size == 1 && group.head.unionGroup.isEmpty) {
        val member = group.head
        val memberContext = s"$context.${member.name}"
        val memberEndian = member.endianOverride.getOrElse(endian)
        val memberCodec =
          compileMemberCodec(member, memberEndian, wordSize, errors, memberContext)
        val memberWidth = memberBitWidth(member, wordSize, errors)
        (memberCodec, memberWidth) match {
          case (Some(codec), Some(width)) =>
            currentOffsetBits += width
            Some(NormalLayoutSlot(CompiledMemberCodec(member.name, width, codec)))
          case _ => None
        }
      } else {
        val compiledMembers = group.flatMap { member =>
          val memberContext = s"$context.${member.name}"
          val memberEndian = member.endianOverride.getOrElse(endian)
          val memberCodec =
            compileMemberCodec(member, memberEndian, wordSize, errors, memberContext)
          val memberWidth = memberBitWidth(member, wordSize, errors)
          (memberCodec, memberWidth) match {
            case (Some(codec), Some(width)) =>
              Some(CompiledMemberCodec(member.name, width, codec))
            case _ => None
          }
        }
        val groupName = group.flatMap(_.unionGroup).headOption.getOrElse("union")
        if (compiledMembers.size == group.size) {
          val width = compiledMembers.map(_.bitWidth).max
          currentOffsetBits += width
          Some(UnionLayoutSlot(groupName, compiledMembers))
        } else {
          None
        }
      }
      paddingSlots ++ memberSlot.toList
    }

    if (compiledSlots.isEmpty) {
      None
    } else {
      val memberBits = compiledSlots.map(_.bitWidth).sum
      val offsetAwareEndBits =
        if (layoutStruct.members.exists(_.offsetBytes.isDefined)) {
          layoutStruct.members.flatMap { member =>
            for {
              offset <- member.offsetBytes
              width <- memberBitWidth(member, wordSize, errors)
            } yield (offset * 8) + width
          }.maxOption
        } else {
          None
        }
      val totalBits = (layoutStruct match {
        case b: IrType.Bitmask            => b.storageBits
        case m: IrType.MemoryMappedStruct => m.declaredSizeBytes.map(_ * 8)
        case e: IrType.EnclosingStruct    => e.declaredSizeBytes.map(_ * 8)
      }).orElse(offsetAwareEndBits).getOrElse(memberBits)

      val resolvedBits = math.max(totalBits, memberBits)
      val paddingBits = resolvedBits - memberBits
      Some(new Codec[Json] {
        override def sizeBound: SizeBound = SizeBound.exact(resolvedBits.toLong)

        override def encode(value: Json): Attempt[BitVector] =
          Attempt.failure(Err("Encoding is not supported for read-only DAP proxy codecs."))

        override def decode(input: BitVector): Attempt[DecodeResult[Json]] = {
          if (input.size < resolvedBits.toLong) {
            Attempt.failure(
              Err(
                s"Insufficient bits for struct decode. Needed $resolvedBits bits, got ${input.size} bits."
              )
            )
          } else {
            val payload = input.take(resolvedBits.toLong)
            val remainder = input.drop(resolvedBits.toLong)
            compiledSlots
              .foldLeft(Attempt.successful((payload, List.empty[(String, Json)]))) {
                case (accAttempt, slot) =>
                  accAttempt.flatMap { case (remainingBits, fields) =>
                    slot match {
                      case PaddingLayoutSlot(paddingBits) =>
                        Attempt.successful((remainingBits.drop(paddingBits.toLong), fields))
                      case NormalLayoutSlot(compiled) =>
                        compiled.codec.decode(remainingBits).map { decoded =>
                          (decoded.remainder, fields :+ (compiled.name -> decoded.value))
                        }
                      case UnionLayoutSlot(_, members) =>
                        val slotBits = members.map(_.bitWidth).max.toLong
                        val slotPayload = remainingBits.take(slotBits)
                        val slotRemainder = remainingBits.drop(slotBits)
                        members
                          .foldLeft(Attempt.successful(List.empty[(String, Json)])) {
                            case (unionAttempt, compiled) =>
                              unionAttempt.flatMap { unionFields =>
                                compiled.codec.decode(slotPayload).map { decoded =>
                                  unionFields :+ (compiled.name -> decoded.value)
                                }
                              }
                          }
                          .map { unionFields =>
                            (slotRemainder, fields ++ unionFields)
                          }
                    }
                  }
              }
              .flatMap { case (remainingAfterMembers, fields) =>
                val remainingAfterPadding = remainingAfterMembers.drop(paddingBits.toLong)
                if (remainingAfterPadding.nonEmpty) {
                  Attempt.failure(Err("Struct decode left unexpected trailing bits."))
                } else {
                  Attempt.successful(DecodeResult(Json.obj(fields: _*), remainder))
                }
              }
          }
        }
      })
    }
  }

  private[daphttp] def compileMemberCodec(
      member: IrMember,
      endian: IrEndian,
      wordSize: Option[Int],
      errors: ListBuffer[String],
      context: String
  ): Option[Codec[Json]] = {
    member.layoutBitWidth
      .filter(_ => memberReadType(member) == IrType.Primitive(IrPrimitive.Bool))
      .map(boolFromStorageCodec)
      .orElse {
        inlineCharByteCount(member).filter(_ > 1).map(inlineCharArrayStringCodec).orElse {
          member.target match {
            case fp: IrType.FunctionPointer =>
              functionPointerCodec(fp, endian, wordSize)
            case listType: IrType.ListType if member.isArray && !member.isPointer =>
              compileArrayCodec(member, listType, endian, wordSize, errors, context)
            case _: IrType.ListType if member.isArray && member.isPointer =>
              compilePointerArrayCodec(member, endian, wordSize, errors, context)
            case _ if member.isPointer =>
              compilePrimitiveCodec(IrPrimitive.LongWord, endian, wordSize)
            case _ =>
              compileJsonCodecForType(memberReadType(member), endian, wordSize, errors, context)
          }
        }
      }
  }

  private[daphttp] def boolFromStorageCodec(storageBits: Int): Codec[Json] =
    bits(storageBits.toLong).xmap[Json](
      value => Json.fromBoolean(bitVectorToUnsigned(value.take(1L)) != 0L),
      _ => BitVector.low(storageBits.toLong)
    )

  private[daphttp] def compilePointerArrayCodec(
      member: IrMember,
      endian: IrEndian,
      wordSize: Option[Int],
      errors: ListBuffer[String],
      context: String
  ): Option[Codec[Json]] = {
    val length = member.arrayLength.orElse {
      errors += s"$context: Pointer arrays must declare @length."
      None
    }
    val elementCodec = compilePrimitiveCodec(IrPrimitive.LongWord, endian, wordSize)
    val elementWidth = IrLayout.bitsForPrimitive(IrPrimitive.LongWord, wordSize)
    (length, elementCodec, elementWidth) match {
      case (Some(count), Some(codec), Some(width)) =>
        val strideBits = arrayElementStrideBits(member, width)
        Some(arrayCodec(count, width, strideBits, codec))
      case _ => None
    }
  }

  private[daphttp] def compileArrayCodec(
      member: IrMember,
      listType: IrType.ListType,
      endian: IrEndian,
      wordSize: Option[Int],
      errors: ListBuffer[String],
      context: String
  ): Option[Codec[Json]] = {
    val length = member.arrayLength.orElse {
      errors += s"$context: Non-pointer arrays must declare @length."
      None
    }
    if (isCharStringArray(member, listType)) {
      length.map(inlineCharArrayStringCodec)
    } else {
      val elementCodec =
        compileJsonCodecForType(listType.element, endian, wordSize, errors, context)
      val elementWidth = listElementBitWidth(listType.element, wordSize).orElse {
        errors += s"$context: Unable to determine array element width."
        None
      }
      (length, elementCodec, elementWidth) match {
        case (Some(count), Some(codec), Some(width)) =>
          val strideBits = arrayElementStrideBits(member, width)
          Some(arrayCodec(count, width, strideBits, codec))
        case _ => None
      }
    }
  }

  private[daphttp] def arrayElementStrideBytes(
      member: IrMember,
      wordSize: Option[Int]
  ): Option[Int] =
    if (!member.isArray) {
      None
    } else {
      val layoutBytes =
        if (member.isPointer) {
          wordSize.map(bits => math.ceil(bits / 8d).toInt)
        } else {
          member.target match {
            case listType: IrType.ListType =>
              listElementBitWidth(listType.element, wordSize).map(bits =>
                math.ceil(bits / 8d).toInt
              )
            case _ =>
              None
          }
        }
      val fromSymbol = for {
        totalBytes <- member.readSizeBytes
        count <- member.arrayLength
        if count > 0 && totalBytes % count == 0
        stride = totalBytes / count
        layout <- layoutBytes
        if stride >= layout
      } yield stride
      fromSymbol.orElse(layoutBytes)
    }

  private[daphttp] def arrayElementStrideBits(member: IrMember, layoutBits: Int): Int =
    (for {
      totalBytes <- member.readSizeBytes
      count <- member.arrayLength
      if count > 0 && (totalBytes * 8) % count == 0
      stride = (totalBytes * 8) / count
      if stride >= layoutBits
    } yield stride).getOrElse(layoutBits)

  private[daphttp] def arrayCodec(
      count: Int,
      elementBits: Int,
      strideBits: Int,
      elementCodec: Codec[Json]
  ): Codec[Json] =
    new Codec[Json] {
      private val totalBits = count.toLong * strideBits.toLong
      require(
        strideBits >= elementBits,
        s"Array stride ($strideBits bits) must cover element width ($elementBits bits)"
      )

      override def sizeBound: SizeBound = SizeBound.exact(totalBits)

      override def encode(value: Json): Attempt[BitVector] =
        Attempt.failure(Err("Encoding is not supported for read-only DAP proxy codecs."))

      override def decode(input: BitVector): Attempt[DecodeResult[Json]] = {
        if (input.size < totalBits) {
          Attempt.failure(
            Err(
              s"Insufficient bits for array decode. Needed $totalBits bits, got ${input.size} bits."
            )
          )
        } else {
          (0 until count)
            .foldLeft(Attempt.successful((input, List.empty[Json]))) { (accAttempt, _) =>
              accAttempt.flatMap { case (remainingBits, elements) =>
                val slot = remainingBits.take(strideBits.toLong)
                val afterSlot = remainingBits.drop(strideBits.toLong)
                elementCodec.decode(slot).map { decoded =>
                  (afterSlot, elements :+ decoded.value)
                }
              }
            }
            .map { case (remainder, elements) =>
              DecodeResult(Json.arr(elements: _*), remainder)
            }
        }
      }
    }

  private[daphttp] def functionPointerCodec(
      fp: IrType.FunctionPointer,
      endian: IrEndian,
      wordSize: Option[Int]
  ): Option[Codec[Json]] = {
    IrLayout.bitsForPrimitive(IrPrimitive.LongWord, wordSize).map { bitWidth =>
      val paramStr = fp.params.map(p => s"${p.typeName} ${p.name}").mkString(", ")
      val prefix = s"<function ${fp.name}($paramStr) @ 0x"
      bits(bitWidth.toLong).xmap[Json](
        value => {
          val normalized = applyEndianToBits(value, bitWidth, endian)
          val raw = bitVectorToUnsigned(normalized)
          // DESNOTE(jbarber, 2026-07-20): Unset callbacks are stored as NULL (0); keep the
          // function shape but show "@ null" instead of "@ 0x0" so empty slots read clearly.
          if (raw == 0L) Json.fromString(s"<function ${fp.name}($paramStr) @ null>")
          else Json.fromString(s"$prefix${raw.toString(16)}>")
        },
        _ => primitiveToBitVector(bitWidth)
      )
    }
  }

  private[daphttp] def compilePrimitiveCodec(
      kind: IrPrimitive,
      endian: IrEndian,
      wordSize: Option[Int]
  ): Option[Codec[Json]] = {
    IrLayout.bitsForPrimitive(kind, wordSize).map { bitWidth =>
      primitiveCodec(kind, bitWidth, endian)
    }
  }

  private[daphttp] def compileIntEnumCodec(
      intEnum: IrType.IntEnum,
      endian: IrEndian,
      wordSize: Option[Int]
  ): Option[Codec[Json]] = {
    IrLayout.bitsForPrimitive(intEnum.underlying, wordSize).map { bitWidth =>
      val namesByValue = intEnum.values.foldLeft(Map.empty[Int, String]) { (acc, enumValue) =>
        if (acc.contains(enumValue.value)) acc else acc + (enumValue.value -> enumValue.name)
      }
      bits(bitWidth.toLong).xmap[Json](
        value => {
          val normalized =
            if (needsEndian(intEnum.underlying)) applyEndianToBits(value, bitWidth, endian)
            else value
          val raw = bitVectorToUnsigned(normalized)
          val signed = signExtend(raw, bitWidth)
          namesByValue.get(signed.intValue) match {
            case Some(name) =>
              Json.fromString(name)
            case None =>
              val mask = (BigInt(1) << bitWidth) - 1
              Json.fromString(f"0x${(raw & mask).toLong}%x")
          }
        },
        _ => primitiveToBitVector(bitWidth)
      )
    }
  }

  private[daphttp] def bitVectorToUnsigned(value: BitVector): BigInt = {
    val bytes = value.toByteArray
    if (bytes.isEmpty) BigInt(0) else BigInt(1, bytes)
  }

  private[daphttp] def signExtend(value: BigInt, bitWidth: Int): BigInt = {
    if (bitWidth <= 0) {
      BigInt(0)
    } else if (value.testBit(bitWidth - 1)) {
      value - (BigInt(1) << bitWidth)
    } else {
      value
    }
  }
}
