package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.BitVector

import java.nio.file.Paths

/** Optional smoke test against a local melee checkout (GALE01). */
class Lbl8046B6A0DecodeSpec extends AnyFunSuite {
  private val meleeRoot = Paths.get(System.getProperty("user.home") + "/projects/ai/yolo/melee")
  private val symbols = meleeRoot.resolve("config/GALE01/symbols.txt")
  private val headers = List(
    meleeRoot.resolve("src"),
    meleeRoot.resolve("extern/dolphin/include")
  )

  test("lbl_8046B6A0_t packs and decodes at symbol size") {
    assume(java.nio.file.Files.exists(symbols), "melee checkout required")
    val generation = DoldecompIrGenerator
      .generateFromPaths(symbols, headers, "doldecomp.generated", "MasterHand", 32)
      .toOption
      .get
    val op = generation.services.head.operations
      .find(_.routePath.contains("lbl_8046B6A0"))
      .getOrElse(fail("no lbl_8046B6A0 op"))
    val root = op.output.members.head.target.asInstanceOf[IrType.MemoryMappedStruct]
    assert(
      root.declaredSizeBytes.contains(0x2528),
      s"packed=${root.declaredSizeBytes.map(b => s"0x${b.toHexString}")}"
    )
    val rules =
      root.members.find(_.name == "x24C8").get.target.asInstanceOf[IrType.MemoryMappedStruct]
    assert(rules.declaredSizeBytes.contains(0x60), s"StartMeleeRules=${rules.declaredSizeBytes}")

    val codec = HttpRouteIrEmitter.compileCodec(root, IrEndian.Big, Some(32))
    assert(codec.isRight, codec)
    val decoded = codec.toOption.get.decode(BitVector(Array.fill[Byte](0x2528)(0)))
    assert(decoded.isSuccessful, decoded.fold(_.message, _ => ""))
  }
}
