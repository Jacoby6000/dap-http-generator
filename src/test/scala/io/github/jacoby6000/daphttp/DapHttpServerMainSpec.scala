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
      |@trait(selector: ":is(member)")
      |structure u64 {}
      |
      |@trait(selector: ":is(member)")
      |structure s64 {}
      |
      |@trait(selector: ":is(member)")
      |structure f8 {}
      |
      |@trait(selector: ":is(member)")
      |structure f16 {}
      |
      |@trait(selector: ":is(member)")
      |structure f32 {}
      |
      |@trait(selector: ":is(member)")
      |structure f64 {}
      |
      |@trait(selector: ":is(member)")
      |structure char {}
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
    assert(route.reads.head.decodeType.nonEmpty)
    assert(route.reads.head.decodeCodec.nonEmpty)
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

  test("allows non-dap primitive members without static addresses") {
    val model = Model
      .assembler()
      .addUnparsedModel("traits.smithy", traitsModel)
      .addUnparsedModel(
        "non-dap-primitive.smithy",
        """$version: "2"
          |
          |namespace example
          |
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
          |    value: Integer
          |}
          |""".stripMargin
      )
      .assemble()
      .unwrap()

    val result = DapHttpServerMain.buildRoutePlansFromModel(model)
    assert(result.isRight)
    assert(result.toOption.get("/Api/GetInfo").reads.isEmpty)
  }

  test("maps u64 trait to primitive IR widths") {
    val model = Model
      .assembler()
      .addUnparsedModel("traits.smithy", traitsModel)
      .addUnparsedModel(
        "u64.smithy",
        """$version: "2"
          |
          |namespace example
          |
          |use com.jacoby6000.daphttp#staticAddress
          |use com.jacoby6000.daphttp#u64
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
          |    @u64
          |    value: Integer
          |}
          |""".stripMargin
      )
      .assemble()
      .unwrap()

    val plans = DapHttpServerMain.buildRoutePlansFromModel(model).toOption.get
    val route = plans("/Api/GetInfo")

    assert(route.reads.head.sizeBytes == 8)
  }

  test("uses service endian while decoding primitive values") {
    val model = Model
      .assembler()
      .addUnparsedModel("traits.smithy", traitsModel)
      .addUnparsedModel(
        "little-endian.smithy",
        """$version: "2"
          |
          |namespace example
          |
          |use com.jacoby6000.daphttp#endian
          |use com.jacoby6000.daphttp#staticAddress
          |use com.jacoby6000.daphttp#u16
          |use com.jacoby6000.daphttp#wordSize
          |
          |@wordSize(32)
          |@endian("little")
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
    val codec = route.reads.head.decodeCodec.get
    val decoded = codec.decode(scodec.bits.BitVector(Array(0x34.toByte, 0x12.toByte))).toOption.get.value

    assert(decoded == io.circe.Json.fromLong(0x1234L))
  }

  test("marks pointer char members as c string pointers") {
    val model = Model
      .assembler()
      .addUnparsedModel("traits.smithy", traitsModel)
      .addUnparsedModel(
        "char-pointer.smithy",
        """$version: "2"
          |
          |namespace example
          |
          |use com.jacoby6000.daphttp#char
          |use com.jacoby6000.daphttp#pointer
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
          |    @pointer
          |    @char
          |    value: Byte
          |}
          |""".stripMargin
      )
      .assemble()
      .unwrap()

    val plans = DapHttpServerMain.buildRoutePlansFromModel(model).toOption.get
    val route = plans("/Api/GetInfo")

    assert(route.reads.head.sizeBytes == 4)
    assert(route.reads.head.cStringPointer)
  }
}
