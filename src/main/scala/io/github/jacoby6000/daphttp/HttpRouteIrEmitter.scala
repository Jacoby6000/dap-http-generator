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

object HttpRouteIrEmitter {
  def emitRoutePlansFromIr(
      irServices: List[IrService]
  ): RoutePlansLoadResult = {
    val errors = ListBuffer.empty[String]
    val routes = ListBuffer.empty[(String, RoutePlan)]

    irServices.foreach { service =>
      service.wordSizeBits match {
        case None =>
          errors += s"${service.name}: Services must declare @wordSize."
          DapHttpLoggers.irEmit.warn("{}: Services must declare @wordSize.", service.name)
        case Some(wordSizeBits) =>
          service.operations.foreach { operation =>
            val operationErrors = ListBuffer.empty[String]
            val reads = collectReadsForType(
              operation.output,
              None,
              operation.routePath,
              service.defaultEndian,
              Some(wordSizeBits),
              operationErrors
            )
            if (operationErrors.nonEmpty) {
              operationErrors.foreach(error =>
                DapHttpLoggers.irEmit.warn("{}: {}", operation.routePath, error)
              )
              errors ++= operationErrors.toList
            } else {
              DapHttpLoggers.irEmit.debug(
                "Compiled route {} with {} read(s)",
                operation.routePath,
                Integer.valueOf(reads.size)
              )
              routes += operation.routePath -> RoutePlan(operation.routePath, reads)
            }
          }
      }
    }

    val result = RoutePlansLoadResult(routes.toMap, errors.toList.distinct)
    DapHttpLoggers.irEmit.info(
      "Emitted {} route(s) with {} error(s)",
      Integer.valueOf(result.routes.size),
      Integer.valueOf(result.errors.size)
    )
    result
  }

  private def collectReadsForType(
      irType: IrType,
      baseAddress: Option[Long],
      pathPrefix: String,
      endian: IrEndian,
      wordSize: Option[Int],
      errors: ListBuffer[String]
  ): List[ReadPlan] = {
    irType match {
      case struct: IrType.Struct =>
        val isDapShape = struct match {
          case _: IrType.Bitmask            => true
          case _: IrType.MemoryMappedStruct => true
          case _: IrType.EnclosingStruct    => false
        }
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
                      endian = endian,
                      wordSizeBits = wordSize,
                      decodeCodec =
                        compileJsonCodec(Some(struct), endian, wordSize, errors, pathPrefix),
                      cStringPointer = false
                    )
                  )
                case None => Nil
              }
          }
        } else {
          struct.members.flatMap { member =>
            val memberPath = s"$pathPrefix.${member.name}"
            val memberRequiresStaticAddress = member.target match {
              case _: IrType.Bitmask            => true
              case _: IrType.MemoryMappedStruct => true
              case _                            => false
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
                collectReadsForType(
                  nestedStruct,
                  memberAddress,
                  memberPath,
                  member.endianOverride.getOrElse(endian),
                  wordSize,
                  errors
                )
              case _ =>
                memberAddress.flatMap { address =>
                  val memberEndian = member.endianOverride.getOrElse(endian)
                  val decodeCodec =
                    compileMemberCodec(member, memberEndian, wordSize, errors, memberPath)
                  memberSizeBytes(member, wordSize, errors).map { sizeBytes =>
                    ReadPlan(
                      path = memberPath,
                      address = address,
                      sizeBytes = sizeBytes,
                      decodeType = Some(memberReadType(member)),
                      endian = memberEndian,
                      wordSizeBits = wordSize,
                      decodeCodec = decodeCodec,
                      cStringPointer =
                        member.isPointer && memberReadType(member) == IrType.Primitive(
                          IrPrimitive.Char
                        )
                    )
                  }
                }.toList
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
        structure match {
          case _: IrType.Bitmask => math.ceil(raw.toDouble / 8d).toInt
          case _                 => raw
        }
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
  ): Option[Int] =
    member.readSizeBytes.orElse {
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

  private def isFloatingPrimitive(kind: IrPrimitive): Boolean = {
    kind match {
      case IrPrimitive.F8 | IrPrimitive.F16 | IrPrimitive.F32 | IrPrimitive.F64 => true
      case _                                                                    => false
    }
  }

  private def isSignedPrimitive(kind: IrPrimitive): Boolean = {
    kind match {
      case IrPrimitive.S8 | IrPrimitive.S16 | IrPrimitive.S32 | IrPrimitive.S64 | IrPrimitive.S128 |
          IrPrimitive.LongWord =>
        true
      case _ => false
    }
  }

  private def needsEndian(kind: IrPrimitive): Boolean = {
    kind match {
      case IrPrimitive.Bool | IrPrimitive.Char | IrPrimitive.F8 => false
      case _                                                    => true
    }
  }

  private def applyEndianToBits(value: BitVector, bitWidth: Int, endian: IrEndian): BitVector = {
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

  private def parseF16(raw: Long): Double = {
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

  private def parseF8(raw: Long): Double = {
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

  private def floatingJson(kind: IrPrimitive, raw: BigInt): Json = {
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

  private def signedJson(bitWidth: Int, raw: BigInt): Json = {
    val value = signExtend(raw, bitWidth)
    if (bitWidth > 64) Json.fromString(value.toString) else Json.fromLong(value.longValue)
  }

  private def unsignedJson(bitWidth: Int, raw: BigInt): Json = {
    if (bitWidth > 63) {
      Json.fromString(raw.toString)
    } else {
      Json.fromLong(raw.longValue)
    }
  }

  private def charJson(raw: BigInt): Json = {
    val c = (raw.longValue & 0xffL).toChar
    Json.fromString(c.toString)
  }

  private def isCharStringArray(member: IrMember, listType: IrType.ListType): Boolean =
    member.isArray && !member.isPointer && listType.element == IrType.Primitive(IrPrimitive.Char)

  private def isCharType(member: IrMember): Boolean =
    memberReadType(member) == IrType.Primitive(IrPrimitive.Char) ||
      member.primitiveOverride.contains(IrPrimitive.Char)

  private def inlineCharByteCount(member: IrMember): Option[Int] =
    if (!isCharType(member) || member.isPointer) {
      None
    } else {
      member.readSizeBytes.orElse {
        if (member.isArray) member.arrayLength else None
      }
    }

  private def nullTerminatedAsciiString(bytes: Array[Byte]): String = {
    val end = bytes.indexWhere(_ == 0)
    val slice = if (end >= 0) bytes.take(end) else bytes
    new String(slice, StandardCharsets.US_ASCII)
  }

  private def bytesFromBitVector(input: BitVector, byteCount: Int): Array[Byte] =
    (0 until byteCount).map { index =>
      bitVectorToUnsigned(input.slice(index * 8L, (index + 1) * 8L)).toByte
    }.toArray

  private def inlineCharArrayStringCodec(count: Int): Codec[Json] =
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

  private def primitiveJson(kind: IrPrimitive, bitWidth: Int, raw: BigInt): Json = {
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

  private def primitiveToBitVector(bitWidth: Int): BitVector = {
    BitVector.low(bitWidth.toLong)
  }

  private def primitiveCodec(kind: IrPrimitive, bitWidth: Int, endian: IrEndian): Codec[Json] =
    bits(bitWidth.toLong).xmap[Json](
      value => {
        val normalized =
          if (needsEndian(kind)) applyEndianToBits(value, bitWidth, endian) else value
        val raw = bitVectorToUnsigned(normalized)
        primitiveJson(kind, bitWidth, raw)
      },
      _ => primitiveToBitVector(bitWidth)
    )

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
      case IrPrimitive.Char     => Some(8)
      case IrPrimitive.U8       => Some(8)
      case IrPrimitive.S8       => Some(8)
      case IrPrimitive.U16      => Some(16)
      case IrPrimitive.S16      => Some(16)
      case IrPrimitive.U32      => Some(32)
      case IrPrimitive.S32      => Some(32)
      case IrPrimitive.U64      => Some(64)
      case IrPrimitive.S64      => Some(64)
      case IrPrimitive.U128     => Some(128)
      case IrPrimitive.S128     => Some(128)
      case IrPrimitive.F8       => Some(8)
      case IrPrimitive.F16      => Some(16)
      case IrPrimitive.F32      => Some(32)
      case IrPrimitive.F64      => Some(64)
      case IrPrimitive.LongWord => wordSize.orElse(Some(64))
    }
  }

  private def memberReadType(member: IrMember): IrType = {
    member.primitiveOverride
      .map(IrType.Primitive.apply)
      .getOrElse(member.target)
  }

  private def compileJsonCodec(
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

  private def compileJsonCodecForType(
      irType: IrType,
      endian: IrEndian,
      wordSize: Option[Int],
      errors: ListBuffer[String],
      context: String
  ): Option[Codec[Json]] = {
    irType match {
      case struct: IrType.Struct =>
        compileStructCodec(struct, endian, wordSize, errors, context)
      case IrType.Primitive(kind) =>
        compilePrimitiveCodec(kind, endian, wordSize)
      case _ =>
        errors += s"$context: Unable to derive decode codec for output type."
        None
    }
  }

  private final case class CompiledMemberCodec(name: String, bitWidth: Int, codec: Codec[Json])

  private def compileStructCodec(
      struct: IrType.Struct,
      endian: IrEndian,
      wordSize: Option[Int],
      errors: ListBuffer[String],
      context: String
  ): Option[Codec[Json]] = {
    val compiledMembers = struct.members.flatMap { member =>
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

    if (compiledMembers.size != struct.members.size) {
      None
    } else {
      val memberBits = compiledMembers.map(_.bitWidth).sum
      val totalBits = struct.declaredSizeBits
        .map(raw =>
          struct match {
            case _: IrType.Bitmask => raw
            case _                 => raw * 8
          }
        )
        .getOrElse(memberBits)

      if (totalBits < memberBits) {
        errors += s"$context: Declared size is smaller than decoded structure members."
        None
      } else {
        val paddingBits = totalBits - memberBits
        Some(new Codec[Json] {
          override def sizeBound: SizeBound = SizeBound.exact(totalBits.toLong)

          override def encode(value: Json): Attempt[BitVector] =
            Attempt.failure(Err("Encoding is not supported for read-only DAP proxy codecs."))

          override def decode(input: BitVector): Attempt[DecodeResult[Json]] = {
            if (input.size < totalBits.toLong) {
              Attempt.failure(
                Err(
                  s"Insufficient bits for struct decode. Needed $totalBits bits, got ${input.size} bits."
                )
              )
            } else {
              val payload = input.take(totalBits.toLong)
              val remainder = input.drop(totalBits.toLong)
              compiledMembers
                .foldLeft(Attempt.successful((payload, List.empty[(String, Json)]))) {
                  case (accAttempt, compiled) =>
                    accAttempt.flatMap { case (remainingBits, fields) =>
                      compiled.codec.decode(remainingBits).map { decoded =>
                        (decoded.remainder, fields :+ (compiled.name -> decoded.value))
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
  }

  private def compileMemberCodec(
      member: IrMember,
      endian: IrEndian,
      wordSize: Option[Int],
      errors: ListBuffer[String],
      context: String
  ): Option[Codec[Json]] = {
    inlineCharByteCount(member).filter(_ > 1).map(inlineCharArrayStringCodec).orElse {
      member.target match {
        case listType: IrType.ListType if member.isArray && !member.isPointer =>
          compileArrayCodec(member, listType, endian, wordSize, errors, context)
        case _ if member.isPointer =>
          compilePrimitiveCodec(IrPrimitive.LongWord, endian, wordSize)
        case _ =>
          compileJsonCodecForType(memberReadType(member), endian, wordSize, errors, context)
      }
    }
  }

  private def compileArrayCodec(
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
        case (Some(count), Some(codec), Some(width)) => Some(arrayCodec(count, width, codec))
        case _                                       => None
      }
    }
  }

  private def arrayCodec(count: Int, elementBits: Int, elementCodec: Codec[Json]): Codec[Json] =
    new Codec[Json] {
      private val totalBits = count.toLong * elementBits.toLong

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
                elementCodec.decode(remainingBits).map { decoded =>
                  (decoded.remainder, elements :+ decoded.value)
                }
              }
            }
            .map { case (remainder, elements) =>
              DecodeResult(Json.arr(elements: _*), remainder)
            }
        }
      }
    }

  private def compilePrimitiveCodec(
      kind: IrPrimitive,
      endian: IrEndian,
      wordSize: Option[Int]
  ): Option[Codec[Json]] = {
    bitsForPrimitive(kind, wordSize).map { bitWidth =>
      primitiveCodec(kind, bitWidth, endian)
    }
  }

  private def bitVectorToUnsigned(value: BitVector): BigInt = {
    val bytes = value.toByteArray
    if (bytes.isEmpty) BigInt(0) else BigInt(1, bytes)
  }

  private def signExtend(value: BigInt, bitWidth: Int): BigInt = {
    if (bitWidth <= 0) {
      BigInt(0)
    } else if (value.testBit(bitWidth - 1)) {
      value - (BigInt(1) << bitWidth)
    } else {
      value
    }
  }
}
