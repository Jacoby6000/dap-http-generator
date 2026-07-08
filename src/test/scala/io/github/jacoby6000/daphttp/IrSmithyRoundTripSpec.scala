package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite
import software.amazon.smithy.model.Model

import java.nio.file.Paths
import scala.jdk.CollectionConverters._

class IrSmithyRoundTripSpec extends AnyFunSuite {
  private val traitsPath = Paths.get("src/main/smithy/dap-http-traits.smithy")

  test("losslessly round trips nested dap structs, lists, unions, and maps") {
    assertLosslessSmithyRoundTrip(
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
  }

  test("losslessly round trips bitmasks") {
    assertLosslessSmithyRoundTrip(
      """$version: "2"
        |
        |namespace example
        |
        |use com.jacoby6000.daphttp#bitmask
        |use com.jacoby6000.daphttp#size
        |use com.jacoby6000.daphttp#staticAddress
        |use com.jacoby6000.daphttp#wordSize
        |
        |@wordSize(32)
        |service Api {
        |    version: "1"
        |    operations: [GetFlags]
        |}
        |
        |operation GetFlags {
        |    output: GetFlagsOutput
        |}
        |
        |structure GetFlagsOutput {
        |    @staticAddress("0x2000")
        |    flags: Flags
        |}
        |
        |@bitmask
        |@size(10)
        |structure Flags {
        |    ready: Boolean
        |    error: Boolean
        |}
        |""".stripMargin
    )
  }

  test("losslessly round trips pointer, char, array, and numeric width traits") {
    assertLosslessSmithyRoundTrip(
      """$version: "2"
        |
        |namespace example
        |
        |use com.jacoby6000.daphttp#array
        |use com.jacoby6000.daphttp#char
        |use com.jacoby6000.daphttp#length
        |use com.jacoby6000.daphttp#pointer
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
        |    @pointer
        |    @char
        |    label: Byte
        |    @staticAddress("0x2000")
        |    @u64
        |    counter: Long
        |    @array
        |    @length(2)
        |    slots: SlotList
        |}
        |
        |list SlotList {
        |    member: Slot
        |}
        |
        |structure Slot {
        |    value: Integer
        |}
        |""".stripMargin
    )
  }

  test("remains lossless across repeated smithy -> IR -> smithy cycles") {
    val originalIr = extractIr(
      """$version: "2"
        |namespace example
        |use com.jacoby6000.daphttp#dapStruct
        |use com.jacoby6000.daphttp#staticAddress
        |use com.jacoby6000.daphttp#wordSize
        |@wordSize(32)
        |service Api { version: "1", operations: [GetInfo] }
        |operation GetInfo { output: GetInfoOutput }
        |structure GetInfoOutput {
        |    @staticAddress("0x1000")
        |    info: Info
        |}
        |@dapStruct
        |structure Info { value: Integer }
        |""".stripMargin
    )

    val once = roundTripIr(originalIr)
    val twice = roundTripIr(once)
    val thrice = roundTripIr(twice)

    IrEquivalence.assertEquivalent(originalIr, once)
    IrEquivalence.assertEquivalent(originalIr, twice)
    IrEquivalence.assertEquivalent(originalIr, thrice)
  }

  private def assertLosslessSmithyRoundTrip(exampleSmithy: String): Unit = {
    val originalIr = extractIr(exampleSmithy)
    val emittedSmithy = IrSmithyEmitter
      .emit(originalIr)
      .fold(
        errors => fail(errors.mkString("\n")),
        identity
      )
    val roundTrippedIr = extractIr(emittedSmithy)
    IrEquivalence.assertEquivalent(originalIr, roundTrippedIr)
  }

  private def roundTripIr(ir: List[IrService]): List[IrService] = {
    val smithy = IrSmithyEmitter.emit(ir).fold(errors => fail(errors.mkString("\n")), identity)
    extractIr(smithy)
  }

  private def extractIr(exampleSmithy: String): List[IrService] =
    SmithyIrGenerator
      .generateFromModel(assembleModel(exampleSmithy))
      .fold(
        errors => fail(errors.mkString("\n")),
        identity
      )

  private def assembleModel(exampleSmithy: String): Model = {
    val result = Model
      .assembler()
      .addImport(traitsPath.toString)
      .addUnparsedModel("example.smithy", exampleSmithy)
      .assemble()
    if (result.isBroken) {
      fail(result.getValidationEvents.asScala.map(_.toString).mkString("\n"))
    } else {
      result.unwrap()
    }
  }
}
