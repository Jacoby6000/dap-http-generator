package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite
import software.amazon.smithy.model.Model

class IrExtractorSpec extends AnyFunSuite {
  private val traitsModel =
    """$version: "2"
      |
      |namespace com.jacoby6000.daphttp
      |
      |@trait(selector: ":is(structure)")
      |structure dapStruct {}
      |
      |@trait(selector: ":is(structure)")
      |structure bitmask {}
      |
      |@trait(selector: ":is(structure)")
      |integer size
      |
      |@trait(selector: ":is(member)")
      |structure pointer {}
      |
      |@trait(selector: ":is(member)")
      |structure array {}
      |
      |@trait(selector: ":is(member)")
      |integer length
      |
      |@trait(selector: ":is(member)")
      |string staticAddress
      |
      |@trait(selector: ":is(service, member)")
      |enum endian {
      |  BIG = "big"
      |  LITTLE = "little"
      |}
      |
      |@trait(selector: ":is(service)")
      |integer wordSize
      |
      |@trait(selector: ":is(member)")
      |structure u16 {}
      |""".stripMargin

  test("extracts service/operation/type IR from smithy model") {
    val model = Model
      .assembler()
      .addUnparsedModel("traits.smithy", traitsModel)
      .addUnparsedModel(
        "example.smithy",
        """$version: "2"
          |
          |namespace example
          |
          |use com.jacoby6000.daphttp#dapStruct
          |use com.jacoby6000.daphttp#endian
          |use com.jacoby6000.daphttp#size
          |use com.jacoby6000.daphttp#staticAddress
          |use com.jacoby6000.daphttp#u16
          |use com.jacoby6000.daphttp#wordSize
          |
          |@wordSize(64)
          |@endian("little")
          |service Api {
          |    version: "1"
          |    operations: [GetSnapshot]
          |}
          |
          |operation GetSnapshot {
          |    output: SnapshotOutput
          |}
          |
          |structure SnapshotOutput {
          |    @staticAddress("0x1000")
          |    registers: RegisterBlock
          |    nested: NestedData
          |}
          |
          |@dapStruct
          |@size(4)
          |structure RegisterBlock {
          |    @u16
          |    lo: Integer
          |    @u16
          |    hi: Integer
          |}
          |
          |structure NestedData {
          |    values: IntList
          |    choice: NumberChoice
          |    lookup: StringToInt
          |}
          |
          |list IntList {
          |    member: Integer
          |}
          |
          |union NumberChoice {
          |    intValue: Integer
          |    strValue: String
          |}
          |
          |map StringToInt {
          |    key: String
          |    value: Integer
          |}
          |""".stripMargin
      )
      .assemble()
      .unwrap()

    val services = IrExtractor.buildIrFromModel(model).toOption.get
    assert(services.size == 1)
    val service = services.head
    assert(service.name == "Api")
    assert(service.wordSizeBits.contains(64))
    assert(service.defaultEndian == IrEndian.Little)
    assert(service.operations.map(_.name) == List("GetSnapshot"))

    val output = service.operations.head.output
    assert(output.isInstanceOf[IrType.EnclosingStruct])

    val registers = output.members.find(_.name == "registers").get
    assert(registers.staticAddress.contains(0x1000L))
    assert(registers.target.isInstanceOf[IrType.MemoryMappedStruct])
    val registerBlock = registers.target.asInstanceOf[IrType.MemoryMappedStruct]
    assert(registerBlock.declaredSizeBits.contains(4))
    assert(
      registerBlock.members.map(_.primitiveOverride) == List(
        Some(IrPrimitive.U16),
        Some(IrPrimitive.U16)
      )
    )

    val nested =
      output.members.find(_.name == "nested").get.target.asInstanceOf[IrType.EnclosingStruct]
    assert(nested.members.find(_.name == "values").get.target.isInstanceOf[IrType.ListType])
    assert(nested.members.find(_.name == "choice").get.target.isInstanceOf[IrType.Union])
    assert(nested.members.find(_.name == "lookup").get.target.isInstanceOf[IrType.MapType])
  }
}
