package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite
import software.amazon.smithy.model.Model

import java.nio.file.Files
import java.nio.file.Paths
import scala.jdk.CollectionConverters._

class SmithyIrEmitterSpec extends AnyFunSuite {
  private val traitsPath = Paths.get("src/main/smithy/dap-http-traits.smithy")

  test("emits Smithy that assembles and round-trips doldecomp IR") {
    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture")
    val originalIr = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot.resolve("include")),
        namespace = "example.doldecomp",
        serviceName = "MeleeApi",
        wordSizeBits = 32
      )
      .toOption
      .get

    val smithyText = SmithyIrEmitter.emit(originalIr).toOption.get
    assert(smithyText.contains("namespace example.doldecomp"))
    assert(smithyText.contains("service MeleeApi"))
    assert(smithyText.contains("@dapStruct"))
    assert(smithyText.contains("@staticAddress(\"0x80453100\")"))

    val roundTrippedIr = assembleAndExtract(smithyText).toOption.get
    IrEquivalence.assertEquivalent(originalIr, roundTrippedIr)
  }

  test("emits Smithy from multi-header doldecomp fixture with pointers and arrays") {
    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture-multi-header")
    val originalIr = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot.resolve("include")),
        namespace = "example.doldecomp.multi",
        serviceName = "MeleeApi",
        wordSizeBits = 32
      )
      .toOption
      .get

    val smithyText = SmithyIrEmitter.emit(originalIr).toOption.get
    assert(smithyText.contains("@pointer"))
    assert(smithyText.contains("@array"))
    assert(smithyText.contains("length(2)"))

    val roundTrippedIr = assembleAndExtract(smithyText).toOption.get
    IrEquivalence.assertEquivalent(originalIr, roundTrippedIr)
  }

  test("writes emitted Smithy to a file path") {
    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture")
    val originalIr = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot.resolve("include")),
        namespace = "example.doldecomp",
        serviceName = "MeleeApi",
        wordSizeBits = 32
      )
      .toOption
      .get

    val outputPath = Files.createTempFile("doldecomp", ".smithy")
    try {
      SmithyIrEmitter.emitToPath(originalIr, outputPath).toOption.get
      val written = Files.readString(outputPath)
      assert(written.contains("service MeleeApi"))
      assembleAndExtract(written).toOption.get
    } finally {
      val _ = Files.deleteIfExists(outputPath)
    }
  }

  private def assembleAndExtract(modelText: String): Either[List[String], List[IrService]] = {
    val assembler = Model
      .assembler()
      .addImport(traitsPath.toString)
      .addUnparsedModel(
        "generated.smithy",
        modelText
      )
    val result = assembler.assemble()
    if (result.isBroken) {
      Left(result.getValidationEvents.iterator().asScala.map(_.toString).toList)
    } else {
      SmithyIrGenerator.generateFromModel(result.unwrap())
    }
  }
}
