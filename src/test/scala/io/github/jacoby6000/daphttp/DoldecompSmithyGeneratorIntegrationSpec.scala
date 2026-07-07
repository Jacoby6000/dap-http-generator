package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite
import software.amazon.smithy.model.Model

import java.nio.file.Paths

class DoldecompSmithyGeneratorIntegrationSpec extends AnyFunSuite {
  test("generates smithy from symbols and headers then compiles route plans") {
    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture")
    val smithy = DoldecompSmithyGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot.resolve("include")),
        namespace = "example.doldecomp",
        serviceName = "MeleeApi",
        wordSizeBits = 32
      )
      .toOption
      .get

    val model = Model
      .assembler()
      .addImport("src/main/smithy/dap-http-traits.smithy")
      .addUnparsedModel("doldecomp-generated.smithy", smithy)
      .assemble()
      .unwrap()

    val plans = DapHttpServerMain.buildRoutePlansFromModel(model).toOption.get
    val route = plans("/MeleeApi/GetGPlayerState")

    assert(route.reads.size == 1)
    assert(route.reads.head.address == 0x80453100L)
    assert(route.reads.head.sizeBytes == 32)
    assert(route.reads.head.decodeCodec.nonEmpty)

    val payload = Array[Byte](
      0x00,
      0x00,
      0x00,
      0x2a,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x05,
      0x3f,
      0x80.toByte,
      0x00,
      0x00,
      0x40,
      0x00,
      0x00,
      0x00,
      0x40,
      0x40,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00
    )
    val decoded = route.reads.head.decodeCodec.get.decode(scodec.bits.BitVector(payload)).toOption.get.value
    val cursor = decoded.hcursor
    assert(cursor.downField("health").as[Long].toOption.contains(42L))
    assert(cursor.downField("score").as[String].toOption.contains("5"))
    assert(cursor.downField("position").downField("x").as[Double].toOption.contains(1.0d))
    assert(cursor.downField("position").downField("y").as[Double].toOption.contains(2.0d))
    assert(cursor.downField("position").downField("z").as[Double].toOption.contains(3.0d))
  }
}
