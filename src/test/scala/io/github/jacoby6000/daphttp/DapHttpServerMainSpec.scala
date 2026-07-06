package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite
import software.amazon.smithy.model.Model

class DapHttpServerMainSpec extends AnyFunSuite {
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
      |integer padding
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
      |@trait(selector: ":is(service)")
      |integer wordSize
      |
      |@trait(selector: ":is(member)")
      |structure cString {
      |  bytes: Integer
      |}
      |
      |@trait(selector: ":is(member)")
      |structure u8 {}
      |
      |@trait(selector: ":is(member)")
      |structure s8 {}
      |
      |@trait(selector: ":is(member)")
      |structure u16 {}
      |
      |@trait(selector: ":is(member)")
      |structure s16 {}
      |
      |@trait(selector: ":is(member)")
      |structure u32 {}
      |
      |@trait(selector: ":is(member)")
      |structure s32 {}
      |
      |list Bytes {
      |  member: Byte
      |}
      |
      |list Bits {
      |  member: Boolean
      |}
      |""".stripMargin

  test("builds read-only route plans for operation outputs that reference dap structs") {
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
          |use com.jacoby6000.daphttp#size
          |use com.jacoby6000.daphttp#staticAddress
          |use com.jacoby6000.daphttp#wordSize
          |
          |@wordSize(32)
          |service Api {
          |    version: "1"
          |    operations: [GetInfo]
          |}
          |
          |operation GetInfo {
          |    output: GetInfoOutput
          |}
          |
          |structure GetInfoOutput {
          |    @staticAddress("0x1000")
          |    info: Info
          |}
          |
          |@dapStruct
          |@size(4)
          |structure Info {
          |    value: Integer
          |}
          |""".stripMargin
      )
      .assemble()
      .unwrap()

    val plans = DapHttpServerMain.buildRoutePlansFromModel(model).toOption.get
    val route = plans("/Api/GetInfo")

    assert(route.reads.size == 1)
    assert(route.reads.head.path == "/Api/GetInfo.info")
    assert(route.reads.head.address == 0x1000L)
    assert(route.reads.head.sizeBytes == 4)
  }

  test("fails planning when non-dap output members do not have static addresses") {
    val model = Model
      .assembler()
      .addUnparsedModel("traits.smithy", traitsModel)
      .addUnparsedModel(
        "missing-address.smithy",
        """$version: "2"
          |
          |namespace example
          |
          |use com.jacoby6000.daphttp#dapStruct
          |use com.jacoby6000.daphttp#size
          |use com.jacoby6000.daphttp#wordSize
          |
          |@wordSize(32)
          |service Api {
          |    version: "1"
          |    operations: [GetInfo]
          |}
          |
          |operation GetInfo {
          |    output: GetInfoOutput
          |}
          |
          |structure GetInfoOutput {
          |    info: Info
          |}
          |
          |@dapStruct
          |@size(4)
          |structure Info {
          |    value: Integer
          |}
          |""".stripMargin
      )
      .assemble()
      .unwrap()

    val result = DapHttpServerMain.buildRoutePlansFromModel(model)
    assert(result.isLeft)
    assert(result.left.toOption.get.exists(_.contains("must declare @staticAddress")))
  }

  test("maps numeric width traits to primitive IR widths") {
    val model = Model
      .assembler()
      .addUnparsedModel("traits.smithy", traitsModel)
      .addUnparsedModel(
        "u16.smithy",
        """$version: "2"
          |
          |namespace example
          |
          |use com.jacoby6000.daphttp#staticAddress
          |use com.jacoby6000.daphttp#u16
          |use com.jacoby6000.daphttp#wordSize
          |
          |@wordSize(32)
          |service Api {
          |    version: "1"
          |    operations: [GetInfo]
          |}
          |
          |operation GetInfo {
          |    output: GetInfoOutput
          |}
          |
          |structure GetInfoOutput {
          |    @staticAddress("0x1000")
          |    @u16
          |    value: Integer
          |}
          |""".stripMargin
      )
      .assemble()
      .unwrap()

    val plans = DapHttpServerMain.buildRoutePlansFromModel(model).toOption.get
    val route = plans("/Api/GetInfo")

    assert(route.reads.size == 1)
    assert(route.reads.head.path == "/Api/GetInfo.value")
    assert(route.reads.head.address == 0x1000L)
    assert(route.reads.head.sizeBytes == 2)
  }
}
