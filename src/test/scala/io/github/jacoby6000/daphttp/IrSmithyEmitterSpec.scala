package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite
import software.amazon.smithy.model.Model

import java.nio.file.Files
import java.nio.file.Paths
import scala.jdk.CollectionConverters._

private final case class NormalizedMember(
    name: String,
    target: String,
    staticAddress: Option[Long],
    isPointer: Boolean,
    isArray: Boolean,
    arrayLength: Option[Int],
    primitiveOverride: Option[IrPrimitive]
)

class IrSmithyEmitterSpec extends AnyFunSuite {
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

    val smithyText = IrSmithyEmitter.emit(originalIr).toOption.get
    assert(smithyText.contains("namespace example.doldecomp"))
    assert(smithyText.contains("service MeleeApi"))
    assert(smithyText.contains("@dapStruct"))
    assert(smithyText.contains("@staticAddress(\"0x80453100\")"))

    val roundTrippedIr = assembleAndExtract(smithyText).toOption.get
    assertEquivalentServices(originalIr, roundTrippedIr)
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

    val smithyText = IrSmithyEmitter.emit(originalIr).toOption.get
    assert(smithyText.contains("@pointer"))
    assert(smithyText.contains("@array"))
    assert(smithyText.contains("@length(2)"))

    val roundTrippedIr = assembleAndExtract(smithyText).toOption.get
    assertEquivalentServices(originalIr, roundTrippedIr)
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
      IrSmithyEmitter.emitToPath(originalIr, outputPath).toOption.get
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
      IrExtractor.buildIrFromModel(result.unwrap())
    }
  }

  private def assertEquivalentServices(
      expected: List[IrService],
      actual: List[IrService]
  ): Unit = {
    assert(actual.map(_.name) == expected.map(_.name))
    assert(actual.map(_.wordSizeBits) == expected.map(_.wordSizeBits))
    assert(actual.map(_.defaultEndian) == expected.map(_.defaultEndian))
    assert(actual.flatMap(_.operations.map(_.name)) == expected.flatMap(_.operations.map(_.name)))
    assert(
      actual.flatMap(_.operations.map(_.routePath)) == expected.flatMap(
        _.operations.map(_.routePath)
      )
    )

    val expectedOutputs = expected.flatMap(_.operations.map(_.output))
    val actualOutputs = actual.flatMap(_.operations.map(_.output))
    assert(actualOutputs.map(structKind) == expectedOutputs.map(structKind))
    val _ = assert(
      actualOutputs.map(_.members.map(normalizeMember)) ==
        expectedOutputs.map(_.members.map(normalizeMember))
    )
  }

  private def structKind(struct: IrType.Struct): String =
    struct match {
      case _: IrType.Bitmask            => "bitmask"
      case _: IrType.MemoryMappedStruct => "memoryMapped"
      case _: IrType.EnclosingStruct    => "enclosing"
    }

  private def normalizeMember(member: IrMember): NormalizedMember =
    NormalizedMember(
      name = member.name,
      target = normalizeType(member.target),
      staticAddress = member.staticAddress,
      isPointer = member.isPointer,
      isArray = member.isArray,
      arrayLength = member.arrayLength,
      primitiveOverride = member.primitiveOverride
    )

  private def normalizeType(irType: IrType): String =
    irType match {
      case struct: IrType.Struct =>
        s"${structKind(struct)}:${struct.id.getName}"
      case union: IrType.Union =>
        s"union:${union.id.getName}"
      case listType: IrType.ListType =>
        s"list:${listType.id.getName}:${normalizeType(listType.element)}"
      case mapType: IrType.MapType =>
        s"map:${mapType.id.getName}"
      case IrType.Ref(id) =>
        s"ref:${id.getName}"
      case IrType.Primitive(kind) =>
        s"primitive:$kind"
    }
}
