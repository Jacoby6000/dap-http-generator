package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files

class DiagnosticSinkSpec extends AnyFunSuite {
  test("collects layout warnings for report without dedicated IrDiagnostics fields") {
    val sink = DiagnosticSink.silent
    sink.warn(
      DiagnosticCategory.Layout,
      "Foo.x: offset comment 0x8 disagrees with type-packed layout 0x4"
    )
    sink.warn(DiagnosticCategory.Section, "Skipping code section .text")
    sink.warn(
      DiagnosticCategory.ArrayBound,
      "Bar.ys: inferred arrayLength=4 from offset comments"
    )

    assert(
      sink.reportOtherWarnings == List(
        "Foo.x: offset comment 0x8 disagrees with type-packed layout 0x4",
        "Bar.ys: inferred arrayLength=4 from offset comments"
      )
    )
    assert(sink.messages(DiagnosticCategory.Section) == List("Skipping code section .text"))
  }

  test("inferred array lengths appear in generation warnings and diagnostics") {
    val tmp = Files.createTempDirectory("dap-diag-array")
    try {
      val header = tmp.resolve("attack.h")
      val symbols = tmp.resolve("symbols.txt")
      Files.writeString(
        header,
        """
          |typedef struct plAttackStats {
          |    /*   +0 */ u32 total;
          |    /*   +4 */ u32 by_attack_counts[StatsAttack_Count];
          |    /* +194 */ u32 thrown_item_count;
          |    /* +198 */ u32 aerials_count;
          |    /* +19C */ u32 specials_count;
          |    /* +1A0 */ u32 x1A0_count;
          |    /* +1A4 */ u32 x1A4_count;
          |    /* +1A8 */ u32 x1A8;
          |} plAttackStats;
          |
          |plAttackStats gAttackStats;
          |""".stripMargin
      )
      Files.writeString(
        symbols,
        "gAttackStats = .data:0x80000000; // type:object size:0x1AC scope:global ctype:plAttackStats\n"
      )

      val generation = DoldecompIrGenerator
        .generateFromPaths(
          symbolsPath = symbols,
          headerRoots = List(tmp),
          namespace = "example.diag",
          serviceName = "Api",
          wordSizeBits = 32
        )
        .toOption
        .get

      assert(generation.warnings.exists(_.contains("inferred arrayLength")))
      assert(generation.diagnostics.otherWarnings.exists(_.contains("inferred arrayLength")))
      assert(generation.warnings.exists(_.contains("StatsAttack_Count")))
    } finally {
      Files.walk(tmp).sorted(java.util.Comparator.reverseOrder()).forEach { path =>
        val _ = Files.deleteIfExists(path)
      }
    }
  }
}
