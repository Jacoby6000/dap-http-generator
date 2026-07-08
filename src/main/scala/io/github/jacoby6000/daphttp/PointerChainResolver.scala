package io.github.jacoby6000.daphttp

object PointerChainResolver {
  def requiredSegmentCount(plan: PointerChainPlan): Int = plan.pointerDepth

  def pointerValue(bytes: Array[Byte], endian: IrEndian): Long = {
    val ordered = endian match {
      case IrEndian.Big    => bytes
      case IrEndian.Little => bytes.reverse
    }
    ordered.foldLeft(0L) { (acc, byte) =>
      (acc << 8) | (byte.toLong & 0xffL)
    }
  }

  def resolveStructAddressFromMemory(
      plan: PointerChainPlan,
      segments: List[Int],
      readMemory: (Long, Int) => Either[String, Array[Byte]]
  ): Either[String, Long] = {
    if (segments.length != requiredSegmentCount(plan)) {
      Left(
        s"Expected ${requiredSegmentCount(plan)} index segment(s) for pointer chain, got ${segments.length}."
      )
    } else {
      val wordBytes = plan.wordSizeBits / 8
      resolveAtFromMemory(
        plan.baseAddress,
        segments,
        plan.outerArrayLength.isDefined,
        wordBytes,
        plan.endian,
        readMemory
      )
    }
  }

  private def resolveAtFromMemory(
      baseAddress: Long,
      segments: List[Int],
      hasOuterArray: Boolean,
      wordBytes: Int,
      endian: IrEndian,
      readMemory: (Long, Int) => Either[String, Array[Byte]]
  ): Either[String, Long] = {
    if (segments.isEmpty) {
      Left("Pointer chain requires at least one index segment.")
    } else if (hasOuterArray) {
      val outerIndex = segments.head
      val innerSegments = segments.tail
      val outerAddress = baseAddress + outerIndex.toLong * wordBytes
      readMemory(outerAddress, wordBytes).map(bytes => pointerValue(bytes, endian)).flatMap {
        pointer =>
          resolveInnerFromMemory(pointer, innerSegments, wordBytes, endian, readMemory)
      }
    } else {
      resolveInnerFromMemory(baseAddress, segments, wordBytes, endian, readMemory)
    }
  }

  private def resolveInnerFromMemory(
      pointer: Long,
      segments: List[Int],
      wordBytes: Int,
      endian: IrEndian,
      readMemory: (Long, Int) => Either[String, Array[Byte]]
  ): Either[String, Long] = {
    segments.foldLeft[Either[String, Long]](Right(pointer)) { case (acc, index) =>
      acc.flatMap { current =>
        readMemory(current + index.toLong * wordBytes, wordBytes).map(bytes =>
          pointerValue(bytes, endian)
        )
      }
    }
  }
}
