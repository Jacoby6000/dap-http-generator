package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite
import software.amazon.smithy.model.Model

class DapStructManifestGeneratorSpec extends AnyFunSuite {
  private val traitsModel =
    """$version: "2"
      |
      |namespace com.jacoby6000.daphttp
      |
      |use smithy.api#default
      |use smithy.api#required
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
      |@trait(selector: ":is(structure, member)")
      |integer alignment
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
      |structure cString {
      |  @required
      |  bytes: Integer
      |
      |  @default("ASCII")
      |  encoding: String
      |}
      |
      |@trait(selector: ":is(member)")
      |structure u8 {}
      |
      |@trait(selector: ":is(member)")
      |structure s16 {}
      |
      |@trait(selector: ":is(member)")
      |structure u32 {}
      |
      |list Bytes {
      |  member: Byte
      |}
      |
      |list Bits {
      |  member: Boolean
      |}
      |""".stripMargin

  test("generates manifest with cString encodings, padding and endian defaults") {
    val model = Model
      .assembler()
      .addUnparsedModel("traits.smithy", traitsModel)
      .addUnparsedModel(
        "example.smithy",
        """$version: "2"
          |
          |namespace example
          |
          |use com.jacoby6000.daphttp#Bytes
          |use com.jacoby6000.daphttp#alignment
          |use com.jacoby6000.daphttp#wordSize
          |use com.jacoby6000.daphttp#cString
          |use com.jacoby6000.daphttp#dapStruct
          |use com.jacoby6000.daphttp#endian
          |use com.jacoby6000.daphttp#padding
          |use com.jacoby6000.daphttp#size
          |use com.jacoby6000.daphttp#u32
          |
          |@wordSize(32)
          |@endian("little")
          |service Api {
          |    version: "1"
          |}
          |
          |@dapStruct
          |@alignment(4)
          |@size(13)
          |structure Frame {
          |    @cString(bytes: 4)
          |    name: String,
          |
          |    @cString(bytes: 2, encoding: "UTF-16LE")
          |    label: String,
          |
          |    @u32
          |    @endian("big")
          |    value: Integer,
          |
          |    @padding(2)
          |    reserved: Bytes
          |}
          |""".stripMargin
      )
      .assemble()
      .unwrap()

    val result = DapStructManifestGenerator.generateWithDiagnostics(model)

    assert(result.errors.isEmpty)
    assert(result.warnings.nonEmpty)
    assert(result.warnings.exists(_.message.contains("only 96 known bits")))

    assert(result.json.contains("\"shapeId\":\"example#Frame\""))
    assert(result.json.contains("\"kind\":\"struct\""))
    assert(result.json.contains("\"size\":13"))
    assert(result.json.contains("\"name\":\"name\",\"type\":\"string\",\"cStringBytes\":4,\"cStringEncoding\":\"ASCII\""))
    assert(result.json.contains("\"name\":\"label\",\"type\":\"string\",\"cStringBytes\":2,\"cStringEncoding\":\"UTF-16LE\""))
    assert(result.json.contains("\"name\":\"reserved\",\"type\":\"Bytes\",\"paddingRepeats\":2,\"endian\":\"little\",\"bitWidth\":16"))
    assert(result.json.contains("\"name\":\"value\",\"type\":\"u32\",\"endian\":\"big\",\"bitWidth\":32"))
  }

  test("validates bitmask size and boolean-member requirements") {
    val model = Model
      .assembler()
      .addUnparsedModel("traits.smithy", traitsModel)
      .addUnparsedModel(
        "bitmask.smithy",
        """$version: "2"
          |
          |namespace example
          |
          |use com.jacoby6000.daphttp#bitmask
          |use com.jacoby6000.daphttp#size
          |
          |@bitmask
          |@size(2)
          |structure Flags {
          |    a: Boolean,
          |    b: Boolean,
          |    c: String
          |}
          |
          |@bitmask
          |structure MissingSize {
          |    enabled: Boolean
          |}
          |
          |@bitmask
          |@size(8)
          |structure Sparse {
          |    enabled: Boolean,
          |    visible: Boolean
          |}
          |""".stripMargin
      )
      .assemble()
      .unwrap()

    val result = DapStructManifestGenerator.generateWithDiagnostics(model)

    assert(result.errors.exists(_.shapeId == "example#Flags$c"))
    assert(result.errors.exists(_.shapeId == "example#Flags"))
    assert(result.errors.exists(_.shapeId == "example#MissingSize"))
    assert(result.warnings.exists(_.shapeId == "example#Sparse"))
    assert(result.json.contains("\"kind\":\"bitmask\""))
  }

  test("validates word size and array length requirements while sizing pointers and longs") {
    val model = Model
      .assembler()
      .addUnparsedModel("traits.smithy", traitsModel)
      .addUnparsedModel(
        "pointer-array.smithy",
        """$version: "2"
          |
          |namespace example
          |
          |use com.jacoby6000.daphttp#array
          |use com.jacoby6000.daphttp#wordSize
          |use com.jacoby6000.daphttp#dapStruct
          |use com.jacoby6000.daphttp#length
          |use com.jacoby6000.daphttp#pointer
          |use com.jacoby6000.daphttp#size
          |
          |service MissingWordSize {
          |    version: "1"
          |}
          |
          |@wordSize(32)
          |service Api {
          |    version: "1"
          |}
          |
          |@dapStruct
          |@size(12)
          |structure Example {
          |    @pointer
          |    data: String,
          |
          |    count: Long,
          |
          |    @array
          |    @length(3)
          |    bytes: BlobList
          |}
          |
          |list BlobList {
          |    member: Byte
          |}
          |
          |@dapStruct
          |structure InvalidArray {
          |    @array
          |    values: BlobList,
          |
          |    @array
          |    @pointer
          |    ptrValues: BlobList
          |}
          |""".stripMargin
      )
      .assemble()
      .unwrap()

    val result = DapStructManifestGenerator.generateWithDiagnostics(model)

    assert(result.errors.exists(_.shapeId == "example#MissingWordSize"))
    assert(result.errors.exists(_.shapeId == "example#InvalidArray$values"))
    assert(result.json.contains("\"name\":\"data\",\"type\":\"pointer\",\"pointer\":true,\"bitWidth\":32"))
    assert(result.json.contains("\"name\":\"count\",\"type\":\"long\",\"bitWidth\":32"))
    assert(result.json.contains("\"name\":\"bytes\",\"type\":\"list\",\"array\":true,\"length\":3,\"bitWidth\":24"))
    assert(!result.errors.exists(_.shapeId == "example#InvalidArray$ptrValues"))
  }
}
