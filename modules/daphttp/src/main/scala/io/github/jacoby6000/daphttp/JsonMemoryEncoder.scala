package io.github.jacoby6000.daphttp

import io.circe.Json
import scodec.bits.BitVector

import java.nio.charset.StandardCharsets
import scala.collection.mutable.ListBuffer

/** Encodes leaf JSON values to memory bytes for DAP `writeMemory`. */
object JsonMemoryEncoder {

  def encode(
      irType: IrType,
      value: Json,
      endian: IrEndian,
      wordSizeBits: Option[Int],
      member: Option[IrMember] = None
  ): Either[String, Array[Byte]] = {
    val effective =
      member.flatMap(_.primitiveOverride).map(IrType.Primitive.apply).getOrElse(irType) match {
        case p: IrType.Primitive => p
        case other               => other
      }
    // Bitfield / sub-byte members need read-modify-write; not supported yet.
    member.flatMap(_.layoutBitWidth) match {
      case Some(bits) if bits % 8 != 0 =>
        Left(s"Bitfield writes (${bits} bits) are not supported yet.")
      case _ =>
        effective match {
          case IrType.Primitive(kind) =>
            encodePrimitive(kind, value, endian, wordSizeBits, member.flatMap(_.readSizeBytes))
          case intEnum: IrType.IntEnum =>
            encodeIntEnum(intEnum, value, endian, wordSizeBits)
          case _: IrType.FunctionPointer =>
            encodePrimitive(IrPrimitive.LongWord, value, endian, wordSizeBits, None)
          case _: IrType.Struct =>
            Left("Cannot write a whole struct; edit a leaf field.")
          case _: IrType.ListType =>
            Left("Cannot write a whole array; edit an element.")
          case IrType.Ref(id) =>
            Left(s"Unresolved type ref $id")
          case other =>
            Left(s"Unsupported write type: $other")
        }
    }
  }

  /** Walk `root` along `segments` (field names / array indices) to the leaf type + relative offset.
    */
  def resolveLeaf(
      root: IrType,
      segments: List[String],
      wordSizeBits: Option[Int]
  ): Either[String, (IrType, IrMember, Int)] = {
    def pack(struct: IrType.Struct): Either[String, IrType.Struct] = {
      val errors = ListBuffer.empty[String]
      val packed = IrLayout.ensureMemberOffsets(struct, wordSizeBits, errors)
      if (packed.members.exists(_.offsetBytes.isEmpty) && errors.nonEmpty)
        Left(errors.toList.distinct.mkString("; "))
      else Right(packed)
    }

    def go(
        current: IrType,
        segs: List[String],
        baseOffset: Int,
        parentMember: Option[IrMember]
    ): Either[String, (IrType, IrMember, Int)] =
      segs match {
        case Nil =>
          parentMember match {
            case Some(m) => Right((current, m, baseOffset))
            case None    => Left("Write requires a field path (segments).")
          }
        case head :: tail =>
          current match {
            case struct: IrType.Struct =>
              pack(struct).flatMap { packed =>
                packed.members.find(_.name == head) match {
                  case None =>
                    Left(s"Unknown field '$head' on ${packed.id}")
                  case Some(member) =>
                    val off = baseOffset + member.offsetBytes.getOrElse(0)
                    if (member.isPointer && tail.nonEmpty) {
                      Left(
                        s"Cannot write through pointer field '${member.name}' via segments; focus the pointee first."
                      )
                    } else {
                      val nextType =
                        member.target match {
                          case list: IrType.ListType if member.isArray => list.element
                          case other                                   => other
                        }
                      if (member.isArray && !member.isPointer) {
                        tail match {
                          case indexStr :: rest if indexStr.forall(_.isDigit) =>
                            indexStr.toIntOption
                              .toRight(s"Invalid or oversized index '$indexStr'")
                              .flatMap { index =>
                                val stride = arrayStrideBytes(member, wordSizeBits).getOrElse(0)
                                val elemOff = off + index * stride
                                if (rest.isEmpty)
                                  Right((nextType, member.copy(isArray = false), elemOff))
                                else
                                  go(nextType, rest, elemOff, Some(member.copy(isArray = false)))
                              }
                          case _ =>
                            Left(s"Array field '${member.name}' requires an index segment.")
                        }
                      } else if (tail.isEmpty) {
                        val writeType =
                          if (member.isPointer) IrType.Primitive(IrPrimitive.LongWord)
                          else
                            member.primitiveOverride
                              .map(IrType.Primitive(_))
                              .getOrElse(nextType)
                        Right((writeType, member, off))
                      } else {
                        go(nextType, tail, off, Some(member))
                      }
                    }
                }
              }
            case list: IrType.ListType =>
              if (head.forall(_.isDigit)) {
                head.toIntOption.toRight(s"Invalid or oversized index '$head'").flatMap { index =>
                  HttpRouteIrEmitter.sizeBytesForType(list.element, wordSizeBits) match {
                    case Right(elemSize) =>
                      go(list.element, tail, baseOffset + index * elemSize, parentMember)
                    case Left(errs) =>
                      Left(errs.mkString("; "))
                  }
                }
              } else Left(s"Expected array index, got '$head'")
            case other =>
              Left(s"Cannot descend into $other at '$head'")
          }
      }

    go(root, segments, 0, None)
  }

  private def arrayStrideBytes(member: IrMember, wordSize: Option[Int]): Option[Int] = {
    val layout =
      if (member.isPointer) wordSize.map(b => (b + 7) / 8)
      else
        member.target match {
          case list: IrType.ListType =>
            HttpRouteIrEmitter.sizeBytesForType(list.element, wordSize).toOption
          case t =>
            HttpRouteIrEmitter.sizeBytesForType(t, wordSize).toOption
        }
    val fromSymbol = for {
      total <- member.readSizeBytes
      count <- member.arrayLength
      if count > 0 && total % count == 0
      stride = total / count
      lay <- layout
      if stride >= lay
    } yield stride
    fromSymbol.orElse(layout)
  }

  private def encodeIntEnum(
      intEnum: IrType.IntEnum,
      value: Json,
      endian: IrEndian,
      wordSizeBits: Option[Int]
  ): Either[String, Array[Byte]] = {
    val raw: Either[String, BigInt] =
      value.asNumber
        .flatMap(n => n.toLong.map(BigInt(_)).orElse(n.toBigDecimal.map(_.toBigInt)))
        .map(Right(_))
        .getOrElse {
          value.asString match {
            case Some(name) if name.startsWith("0x") || name.startsWith("0X") =>
              try Right(BigInt(name.drop(2), 16))
              catch { case _: NumberFormatException => Left(s"Invalid hex enum value: $name") }
            case Some(name) =>
              intEnum.values
                .find(_.name == name)
                .map(v => Right(BigInt(v.value)))
                .getOrElse(Left(s"Unknown enumerator '$name' for ${intEnum.id}"))
            case None =>
              Left("Enum value must be a name, number, or hex string.")
          }
        }
    raw.flatMap(v =>
      encodePrimitive(intEnum.underlying, Json.fromBigInt(v), endian, wordSizeBits, None)
    )
  }

  private def encodePrimitive(
      kind: IrPrimitive,
      value: Json,
      endian: IrEndian,
      wordSizeBits: Option[Int],
      readSizeBytes: Option[Int]
  ): Either[String, Array[Byte]] = {
    bitsFor(kind, wordSizeBits) match {
      case None =>
        Left(s"Unable to determine width for $kind")
      case Some(bitWidth) =>
        kind match {
          case IrPrimitive.Char if value.isString =>
            val text = value.asString.getOrElse("")
            val size = readSizeBytes.filter(_ > 0).getOrElse(math.max(1, (bitWidth + 7) / 8))
            val bytes = text.getBytes(StandardCharsets.US_ASCII).take(size)
            val out = Array.fill[Byte](size)(0)
            System.arraycopy(bytes, 0, out, 0, bytes.length)
            Right(out)
          case _ =>
            toRawBits(kind, value, bitWidth).map { raw =>
              val bits = bigIntToBits(raw, bitWidth)
              val ordered = applyEndian(bits, bitWidth, endian, kind)
              ordered.toByteArray match {
                case arr if arr.length == bitWidth / 8 => arr
                case arr if arr.length < bitWidth / 8  =>
                  Array.fill[Byte](bitWidth / 8 - arr.length)(0) ++ arr
                case arr =>
                  arr.takeRight(bitWidth / 8)
              }
            }
        }
    }
  }

  private def toRawBits(kind: IrPrimitive, value: Json, bitWidth: Int): Either[String, BigInt] =
    kind match {
      case IrPrimitive.Bool =>
        value.asBoolean
          .map(b => Right(if (b) BigInt(1) else BigInt(0)))
          .orElse(
            value.asNumber.flatMap(_.toInt).map(i => Right(if (i != 0) BigInt(1) else BigInt(0)))
          )
          .getOrElse(Left("Boolean field expects true/false or 0/1."))
      case IrPrimitive.F32 =>
        value.asNumber
          .map(_.toDouble)
          .map(d => Right(BigInt(java.lang.Float.floatToIntBits(d.toFloat).toLong & 0xffffffffL)))
          .getOrElse(Left("f32 expects a number."))
      case IrPrimitive.F64 =>
        value.asNumber
          .map(_.toDouble)
          .map(d => Right(BigInt(java.lang.Double.doubleToLongBits(d))))
          .getOrElse(Left("f64 expects a number."))
      case IrPrimitive.F16 | IrPrimitive.F8 =>
        Left(s"$kind writes are not supported yet.")
      case IrPrimitive.Char =>
        value.asString
          .map { s =>
            if (s.isEmpty) Right(BigInt(0))
            else Right(BigInt(s.charAt(0).toInt & 0xff))
          }
          .orElse(value.asNumber.flatMap(_.toInt).map(i => Right(BigInt(i & 0xff))))
          .getOrElse(Left("char expects a string or number."))
      case _ =>
        value.asNumber
          .flatMap(n => n.toLong.map(BigInt(_)).orElse(n.toBigDecimal.map(_.toBigInt)))
          .map { v =>
            val mask = (BigInt(1) << bitWidth) - 1
            Right(v & mask)
          }
          .orElse {
            value.asString.collect {
              case s if s.startsWith("0x") || s.startsWith("0X") =>
                try {
                  val mask = (BigInt(1) << bitWidth) - 1
                  Right(BigInt(s.drop(2), 16) & mask)
                } catch {
                  case _: NumberFormatException => Left(s"Invalid hex: $s")
                }
            }
          }
          .getOrElse(Left(s"$kind expects a number (or hex string)."))
    }

  private def bitsFor(kind: IrPrimitive, wordSize: Option[Int]): Option[Int] =
    kind match {
      case IrPrimitive.Bool                                                    => Some(8)
      case IrPrimitive.Char | IrPrimitive.U8 | IrPrimitive.S8 | IrPrimitive.F8 => Some(8)
      case IrPrimitive.U16 | IrPrimitive.S16 | IrPrimitive.F16                 => Some(16)
      case IrPrimitive.U32 | IrPrimitive.S32 | IrPrimitive.F32                 => Some(32)
      case IrPrimitive.U64 | IrPrimitive.S64 | IrPrimitive.F64                 => Some(64)
      case IrPrimitive.U128 | IrPrimitive.S128                                 => Some(128)
      case IrPrimitive.LongWord                                                => wordSize
      case _                                                                   => None
    }

  private def bigIntToBits(value: BigInt, bitWidth: Int): BitVector = {
    val byteCount = (bitWidth + 7) / 8
    val unsigned = if (value < 0) value + (BigInt(1) << bitWidth) else value
    val bytes = unsigned.toByteArray
    val padded =
      if (bytes.length >= byteCount) bytes.takeRight(byteCount)
      else Array.fill[Byte](byteCount - bytes.length)(0) ++ bytes
    BitVector(padded).take(bitWidth.toLong)
  }

  private def applyEndian(
      value: BitVector,
      bitWidth: Int,
      endian: IrEndian,
      kind: IrPrimitive
  ): BitVector = {
    val needs =
      kind match {
        case IrPrimitive.Bool | IrPrimitive.Char | IrPrimitive.F8 => false
        case _                                                    => true
      }
    if (!needs || bitWidth % 8 != 0 || bitWidth <= 8 || endian == IrEndian.Big) value
    else {
      val byteCount = bitWidth / 8
      val bytes = value.take(bitWidth.toLong).toByteArray
      val padded =
        if (bytes.length == byteCount) bytes
        else Array.fill[Byte](byteCount - bytes.length)(0) ++ bytes
      BitVector(padded.reverse)
    }
  }
}
