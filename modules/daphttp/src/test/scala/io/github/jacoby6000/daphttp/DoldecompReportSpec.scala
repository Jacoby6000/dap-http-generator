package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files

class DoldecompReportSpec extends AnyFunSuite {
  test("writes a detailed report with full skip and conflict lists") {
    val reportPath = Files.createTempFile("dap-report", ".md")
    try {
      val diagnostics = IrDiagnostics(
        codeSectionSkips = List(".text" -> List("gCode", "gInit")),
        unresolvedSymbols = List("orphanA", "orphanB"),
        missingTypes = List("Vec3" -> List("g_vec")),
        conflictingMacros = List(
          NamedConflict("MAX", keptSource = "/a.h", ignoredSources = List("/b.h"))
        ),
        headerRoots = List("/headers"),
        sourceFileCount = 2,
        symbolCount = 10,
        dataObjectCount = 8,
        resolvedSymbolCount = 5,
        operationCount = 4
      )
      val summary = List("Skipping 2 object symbol(s) in known code section(s): '.text' (2).")
      val written = DoldecompReport.write(reportPath, diagnostics, summary)
      val text = Files.readString(written)
      assert(text.contains("# dap-http-generator cheaders report"))
      assert(text.contains("### `.text` (2)"))
      assert(text.contains("- gCode"))
      assert(text.contains("- orphanA"))
      assert(text.contains("### `Vec3` (1)"))
      assert(text.contains("### `MAX`"))
      assert(text.contains("- kept: /a.h"))
      assert(text.contains("- ignored: /b.h"))
      assert(text.contains(summary.head))
    } finally {
      val _ = Files.deleteIfExists(reportPath)
    }
  }

  test("other warnings omit messages already listed in the summary") {
    val reportPath = Files.createTempFile("dap-report-dedupe", ".md")
    try {
      val shared = "Foo.x: offset comment 0x8 disagrees with type-packed layout 0x4"
      val diagnostics = IrDiagnostics(otherWarnings = List(shared, "only-in-other"))
      val text = Files.readString(DoldecompReport.write(reportPath, diagnostics, List(shared)))
      val otherSection = text.split("## Other warnings", 2)(1)
      assert(otherSection.contains("only-in-other"))
      assert(!otherSection.contains(shared))
      assert(text.contains(shared)) // still present under Summary warnings
    } finally {
      val _ = Files.deleteIfExists(reportPath)
    }
  }

  test("SectionFilter exposes detailed code-section skip lists for reports") {
    val symbols = List(
      DoldecompSymbol(
        name = "gData",
        section = ".data",
        address = 0x8000L,
        attributes = Map("type" -> "object", "size" -> "0x4")
      ),
      DoldecompSymbol(
        name = "gCode",
        section = ".text",
        address = 0x8001L,
        attributes = Map("type" -> "object", "size" -> "0x4")
      ),
      DoldecompSymbol(
        name = "gInit",
        section = ".init",
        address = 0x8002L,
        attributes = Map("type" -> "object", "size" -> "0x4")
      )
    )
    val result = SectionFilter.filterDataSymbols(symbols)
    assert(result.codeSectionSkips.exists { case (section, names) =>
      section == ".text" && names == List("gCode")
    })
    assert(result.codeSectionSkips.exists { case (section, names) =>
      section == ".init" && names == List("gInit")
    })
  }
}
