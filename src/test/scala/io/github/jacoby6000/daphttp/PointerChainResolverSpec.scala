package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite
import software.amazon.smithy.model.shapes.ShapeId

class PointerChainResolverSpec extends AnyFunSuite {
  private val pointeeStruct = IrType.MemoryMappedStruct(
    id = ShapeId.from("test#EventInitDataLevelTbl"),
    members = List(
      IrMember(
        id = ShapeId.from("test#EventInitDataLevelTbl$kind"),
        name = "kind",
        target = IrType.Primitive(IrPrimitive.U8),
        staticAddress = None,
        paddingRepeats = None,
        isPointer = false,
        isArray = false,
        arrayLength = None,
        endianOverride = None,
        primitiveOverride = Some(IrPrimitive.U8)
      )
    ),
    declaredSizeBits = None
  )

  private val chainPlan = PointerChainPlan(
    pointeeType = pointeeStruct,
    pointerDepth = 2,
    outerArrayLength = Some(2),
    baseAddress = 0x804d6900L,
    endian = IrEndian.Big,
    wordSizeBits = 32,
    pointeeSizeBytes = 1,
    pointeeDecodeCodec = None
  )

  test("resolves pointer chain addresses from mocked memory") {
    val memory = Map[Long, Array[Byte]](
      0x804d6900L -> Array[Byte](0x80.toByte, 0x00, 0x10, 0x00),
      0x80001004L -> Array[Byte](0x80.toByte, 0x00, 0x20, 0x04),
      0x80002004L -> Array[Byte](0x42)
    )

    def readMemory(address: Long, sizeBytes: Int): Either[String, Array[Byte]] =
      memory.get(address).filter(_.length >= sizeBytes).toRight(s"missing memory at 0x$address%x")

    val resolved =
      PointerChainResolver.resolveStructAddressFromMemory(chainPlan, List(0, 1), readMemory)

    assert(resolved.contains(0x80002004L))
  }
}
