package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite
import software.amazon.smithy.model.shapes.ShapeId

class HttpRouteIrEmitterRouteSpec extends AnyFunSuite {
  private def id(value: String): ShapeId = ShapeId.from(s"example#$value")

  test("compiles IR into route reads for dap-backed members") {
    val registerStruct = IrType.MemoryMappedStruct(
      id = id("RegisterBlock"),
      members = List(
        IrMember(
          id = id("RegisterBlock_lo"),
          name = "lo",
          target = IrType.Primitive(IrPrimitive.U16),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = Some(IrPrimitive.U16)
        ),
        IrMember(
          id = id("RegisterBlock_hi"),
          name = "hi",
          target = IrType.Primitive(IrPrimitive.U16),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = Some(IrPrimitive.U16)
        )
      ),
      declaredSizeBits = Some(4)
    )

    val output = IrType.EnclosingStruct(
      id = id("GetSnapshotOutput"),
      members = List(
        IrMember(
          id = id("GetSnapshotOutput_registers"),
          name = "registers",
          target = registerStruct,
          staticAddress = Some(0x1000L),
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = None
        )
      ),
      declaredSizeBits = None
    )

    val plans = HttpRouteIrEmitter
      .emitRoutePlansFromIr(
        List(
          IrService(
            name = "Api",
            wordSizeBits = Some(32),
            defaultEndian = IrEndian.Big,
            operations = List(
              IrOperation(name = "GetSnapshot", routePath = "/api/Api/GetSnapshot", output = output)
            )
          )
        )
      )
      .routes

    val read = plans("/api/Api/GetSnapshot").reads.head
    assert(read.path == "/api/Api/GetSnapshot.registers")
    assert(read.address == 0x1000L)
    assert(read.sizeBytes == 4)
    assert(read.decodeCodec.nonEmpty)
  }

  test("compiles bitmask size from bits into route read bytes") {
    val flags = IrType.Bitmask(
      id = id("Flags"),
      members = List(
        IrMember(
          id = id("Flags_ready"),
          name = "ready",
          target = IrType.Primitive(IrPrimitive.Bool),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = None
        ),
        IrMember(
          id = id("Flags_error"),
          name = "error",
          target = IrType.Primitive(IrPrimitive.Bool),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = None
        )
      ),
      declaredSizeBits = Some(10)
    )

    val output = IrType.EnclosingStruct(
      id = id("GetFlagsOutput"),
      members = List(
        IrMember(
          id = id("GetFlagsOutput_flags"),
          name = "flags",
          target = flags,
          staticAddress = Some(0x2000L),
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = None
        )
      ),
      declaredSizeBits = None
    )

    val plans = HttpRouteIrEmitter
      .emitRoutePlansFromIr(
        List(
          IrService(
            name = "Api",
            wordSizeBits = Some(32),
            defaultEndian = IrEndian.Big,
            operations =
              List(IrOperation(name = "GetFlags", routePath = "/api/Api/GetFlags", output = output))
          )
        )
      )
      .routes

    val read = plans("/api/Api/GetFlags").reads.head
    assert(read.sizeBytes == 2)
  }
}
