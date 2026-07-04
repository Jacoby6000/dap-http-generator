package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite
import software.amazon.smithy.model.Model

class DapStructManifestGeneratorSpec extends AnyFunSuite {
  private val traitsModel =
    """$version: "2"
      |
      |namespace com.jacoby6000.daphttp
      |
      |@trait(selector: ":is(structure)")
      |structure dapStruct {}
      |
      |@trait(selector: ":is(structure, member)")
      |integer alignment
      |
      |@trait(selector: ":is(member)")
      |integer cString
      |
      |@trait(selector: ":is(member)")
      |structure u8 {}
      |
      |@trait(selector: ":is(member)")
      |structure s16 {}
      |
      |@trait(selector: ":is(member)")
      |structure u32 {}
      |""".stripMargin

  test("generates DAP manifest for dapStruct structures") {
    val model = Model
      .assembler()
      .addUnparsedModel("traits.smithy", traitsModel)
      .addUnparsedModel(
        "example.smithy",
        """$version: "2"
          |
          |namespace example
          |
          |use com.jacoby6000.daphttp#alignment
          |use com.jacoby6000.daphttp#cString
          |use com.jacoby6000.daphttp#dapStruct
          |use com.jacoby6000.daphttp#s16
          |use com.jacoby6000.daphttp#u32
          |use com.jacoby6000.daphttp#u8
          |
          |@dapStruct
          |@alignment(4)
          |structure Frame {
          |    @u8
          |    kind: Byte,
          |    @s16
          |    offset: Short,
          |    @u32
          |    @alignment(4)
          |    length: Integer,
          |    @cString(32)
          |    name: String
          |}
          |
          |structure Ignored {
          |   value: String
          |}
          |""".stripMargin
      )
      .assemble()
      .unwrap()

    val manifest = DapStructManifestGenerator.generate(model)

    assert(manifest.contains("\"shapeId\":\"example#Frame\""))
    assert(manifest.contains("\"alignment\":4"))
    assert(manifest.contains("\"name\":\"kind\",\"type\":\"u8\""))
    assert(manifest.contains("\"name\":\"offset\",\"type\":\"s16\""))
    assert(manifest.contains("\"name\":\"length\",\"type\":\"u32\",\"alignment\":4"))
    assert(manifest.contains("\"name\":\"name\",\"type\":\"string\",\"cStringBytes\":32"))
    assert(!manifest.contains("Ignored"))
  }
}
