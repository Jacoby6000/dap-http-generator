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

    val (dataSymbols, nonDataSymbols) = objectSymbols.partition { symbol =>
      val section = normalize(symbol.section)
      knownData.contains(section) || looksLikeData(section)
    }

    val codeSectionSymbols = nonDataSymbols.filter { symbol =>
      NonDataSections.contains(normalize(symbol.section))
    }
    val unknownSectionSymbols = nonDataSymbols.filterNot { symbol =>
      NonDataSections.contains(normalize(symbol.section))
    }

    val unknownSections = unknownSectionSymbols
      .map(s => normalize(s.section))
      .filterNot(knownData.contains)
      .filterNot(looksLikeData)
      .distinct
      .sorted

    val unknownWarnings = unknownSections.map { section =>
      s"Unknown section '$section' is not recognized as data or code; symbols in it will be skipped. Use --data-sections to include it."
    }
    val codeWarnings =
      if (codeSectionSymbols.isEmpty) {
        Nil
      } else {
        val bySection = codeSectionSymbols.groupBy(s => normalize(s.section)).toList.sortBy(_._1)
        bySection.map { case (section, symbols) =>
          val names = symbols.map(_.name).sorted.mkString(", ")
          s"Skipping ${symbols.size} object symbol(s) in code section '$section': $names. Use --data-sections to include it."
        }
      }

    SectionFilterResult(dataSymbols, unknownWarnings ++ codeWarnings)
  }

  private def normalize(section: String): String =
    section.trim.toLowerCase

  private def looksLikeData(section: String): Boolean =
    section.endsWith("data") || section.endsWith("bss") || section.contains("rodata")
}
