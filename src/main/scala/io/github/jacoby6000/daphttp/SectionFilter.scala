package io.github.jacoby6000.daphttp

object SectionFilter {

  val DefaultDataSections: Set[String] =
    Set(".data", ".sdata", ".sdata2", ".sbss", ".bss", ".rodata")

  private val NonDataSections: Set[String] =
    Set(".text", ".init", ".ctors", ".dtors", "extab", "extabindex")

  final case class SectionFilterResult(
      dataSymbols: List[DoldecompSymbol],
      warnings: List[String]
  )

  def filterDataSymbols(
      symbols: List[DoldecompSymbol],
      extraSections: Set[String] = Set.empty
  ): SectionFilterResult = {
    val knownData = DefaultDataSections ++ extraSections.map(normalize)
    val objectSymbols = symbols.filter(_.symbolType.contains("object"))

    val (dataSymbols, unknownSymbols) = objectSymbols.partition { symbol =>
      val section = normalize(symbol.section)
      knownData.contains(section) || looksLikeData(section)
    }

    val unknownSections = unknownSymbols
      .map(s => normalize(s.section))
      .filterNot(knownData.contains)
      .filterNot(NonDataSections.contains)
      .filterNot(looksLikeData)
      .distinct
      .sorted

    val warnings = unknownSections.map { section =>
      s"Unknown section '$section' is not recognized as data or code; symbols in it will be skipped. Use --data-sections to include it."
    }

    SectionFilterResult(dataSymbols, warnings)
  }

  private def normalize(section: String): String =
    section.trim.toLowerCase

  private def looksLikeData(section: String): Boolean =
    section.endsWith("data") || section.endsWith("bss") || section.contains("rodata")
}
