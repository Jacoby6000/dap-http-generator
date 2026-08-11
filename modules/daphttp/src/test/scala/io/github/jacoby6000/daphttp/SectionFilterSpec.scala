package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite

class SectionFilterSpec extends AnyFunSuite {
  test("keeps data-section objects and warns about code-section objects") {
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
        attributes = Map("type" -> "object", "size" -> "0x4", "ctype" -> "Foo")
      ),
      DoldecompSymbol(
        name = "gInit",
        section = ".init",
        address = 0x8002L,
        attributes = Map("type" -> "object", "size" -> "0x4")
      )
    )

    val result = SectionFilter.filterDataSymbols(symbols)

    assert(result.dataSymbols.map(_.name) == List("gData"))
    assert(result.warnings.exists(_.contains("known code section")))
    assert(result.warnings.exists(_.contains(".text")))
    assert(result.warnings.exists(_.contains(".init")))
    assert(!result.warnings.exists(_.contains("--data-sections")))
  }

  test("warns about unknown non-data sections") {
    val symbols = List(
      DoldecompSymbol(
        name = "gWeird",
        section = ".custom",
        address = 0x8000L,
        attributes = Map("type" -> "object", "size" -> "0x4")
      )
    )

    val result = SectionFilter.filterDataSymbols(symbols)

    assert(result.dataSymbols.isEmpty)
    assert(result.warnings.exists(_.contains(".custom")))
  }
}
