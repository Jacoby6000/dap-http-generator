package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite
import software.amazon.smithy.model.shapes.ShapeId

class IrSizingWarningsSpec extends AnyFunSuite {
  private def id(value: String): ShapeId = ShapeId.from(s"example#$value")

  test("warns on plain Integer members without width traits") {
    val services = List(
      serviceWithOutput(
        IrType.EnclosingStruct(
          id = id("GetInfoOutput"),
          members = List(
            member("value", IrType.Primitive(IrPrimitive.S32), staticAddress = Some(0x1000L))
          ),
          declaredSizeBytes = None
        )
      )
    )

    val warnings = IrSizingWarnings.collect(services)
    assert(warnings.nonEmpty)
    assert(warnings.head.contains("Integer member lacks an explicit width trait"))
  }

  test("warns on plain Long members without width traits") {
    val services = List(
      serviceWithOutput(
        IrType.EnclosingStruct(
          id = id("GetInfoOutput"),
          members = List(
            member("value", IrType.Primitive(IrPrimitive.LongWord), staticAddress = Some(0x1000L))
          ),
          declaredSizeBytes = None
        )
      )
    )

    val warnings = IrSizingWarnings.collect(services)
    assert(warnings.nonEmpty)
    assert(warnings.head.contains("Long member lacks an explicit width trait"))
  }

  test("warns on plain Float members without width traits") {
    val services = List(
      serviceWithOutput(
        IrType.EnclosingStruct(
          id = id("GetInfoOutput"),
          members = List(
            member("value", IrType.Primitive(IrPrimitive.F32), staticAddress = Some(0x1000L))
          ),
          declaredSizeBytes = None
        )
      )
    )

    val warnings = IrSizingWarnings.collect(services)
    assert(warnings.nonEmpty)
    assert(warnings.head.contains("Float member lacks an explicit width trait"))
  }

  test("warns on plain Double members without width traits") {
    val services = List(
      serviceWithOutput(
        IrType.EnclosingStruct(
          id = id("GetInfoOutput"),
          members = List(
            member("value", IrType.Primitive(IrPrimitive.F64), staticAddress = Some(0x1000L))
          ),
          declaredSizeBytes = None
        )
      )
    )

    val warnings = IrSizingWarnings.collect(services)
    assert(warnings.nonEmpty)
    assert(warnings.head.contains("Double member lacks an explicit width trait"))
  }

  test("does not warn when Float members declare width traits") {
    val services = List(
      serviceWithOutput(
        IrType.EnclosingStruct(
          id = id("GetInfoOutput"),
          members = List(
            member(
              "value",
              IrType.Primitive(IrPrimitive.F32),
              staticAddress = Some(0x1000L),
              primitiveOverride = Some(IrPrimitive.F16)
            )
          ),
          declaredSizeBytes = None
        )
      )
    )

    assert(IrSizingWarnings.collect(services).isEmpty)
  }

  test("does not warn on explicitly sized C-style float targets") {
    val services = List(
      serviceWithOutput(
        IrType.EnclosingStruct(
          id = id("GetInfoOutput"),
          members = List(
            member(
              "position",
              IrType.Primitive(IrPrimitive.F32),
              staticAddress = Some(0x1000L),
              primitiveOverride = Some(IrPrimitive.F32)
            )
          ),
          declaredSizeBytes = None
        )
      )
    )

    assert(IrSizingWarnings.collect(services).isEmpty)
  }

  test("does not warn when Integer members declare width traits") {
    val services = List(
      serviceWithOutput(
        IrType.EnclosingStruct(
          id = id("GetInfoOutput"),
          members = List(
            member(
              "value",
              IrType.Primitive(IrPrimitive.S32),
              staticAddress = Some(0x1000L),
              primitiveOverride = Some(IrPrimitive.U32)
            )
          ),
          declaredSizeBytes = None
        )
      )
    )

    assert(IrSizingWarnings.collect(services).isEmpty)
  }

  test("does not warn on pointer members that follow service word size") {
    val services = List(
      serviceWithOutput(
        IrType.EnclosingStruct(
          id = id("GetInfoOutput"),
          members = List(
            member(
              "value",
              IrType.Primitive(IrPrimitive.LongWord),
              staticAddress = Some(0x1000L),
              isPointer = true,
              primitiveOverride = Some(IrPrimitive.Char)
            )
          ),
          declaredSizeBytes = None
        )
      )
    )

    assert(IrSizingWarnings.collect(services).isEmpty)
  }

  test("does not warn on explicitly sized C-style primitive targets") {
    val services = List(
      serviceWithOutput(
        IrType.EnclosingStruct(
          id = id("GetInfoOutput"),
          members = List(
            member(
              "health",
              IrType.Primitive(IrPrimitive.U32),
              staticAddress = Some(0x1000L)
            )
          ),
          declaredSizeBytes = None
        )
      )
    )

    assert(IrSizingWarnings.collect(services).isEmpty)
  }

  test("warns on nested dap struct members without width traits") {
    val services = List(
      serviceWithOutput(
        IrType.EnclosingStruct(
          id = id("GetInfoOutput"),
          members = List(
            member(
              "info",
              IrType.MemoryMappedStruct(
                id = id("Info"),
                members = List(member("value", IrType.Primitive(IrPrimitive.S32))),
                declaredSizeBytes = Some(4)
              ),
              staticAddress = Some(0x1000L)
            )
          ),
          declaredSizeBytes = None
        )
      )
    )

    val warnings = IrSizingWarnings.collect(services)
    assert(warnings.exists(_.contains("Integer member lacks an explicit width trait")))
    assert(warnings.size == 1)
  }

  private def serviceWithOutput(output: IrType.Struct): IrService =
    IrService(
      name = "Api",
      wordSizeBits = Some(32),
      defaultEndian = IrEndian.Big,
      operations = List(
        IrOperation(
          name = "GetInfo",
          routePath = "/api/Api/GetInfo",
          output = output
        )
      )
    )

  private def member(
      name: String,
      target: IrType,
      staticAddress: Option[Long] = None,
      isPointer: Boolean = false,
      primitiveOverride: Option[IrPrimitive] = None
  ): IrMember =
    IrMember(
      id = id(s"Output$$$name"),
      name = name,
      target = target,
      staticAddress = staticAddress,
      paddingRepeats = None,
      isPointer = isPointer,
      isArray = false,
      arrayLength = None,
      endianOverride = None,
      primitiveOverride = primitiveOverride
    )
}
