package io.github.jacoby6000.daphttp

import scala.collection.mutable

/** Multi-pass enum harvesting (Count sentinels) for the cheaders frontend. */
private[daphttp] object CheadersEnumCorpus {

  def load(
      corpus: HeaderCorpus,
      macros: Map[String, String]
  ): EnumParseResult = {
    // DESNOTE(jbarber, 2026-07-20): Character motion enums form a dependency chain
    // (ftCo_MS_Count → ftMh_MS_Count → ftCh_MS_Count). Iterate: parse → harvest Count sentinels that
    // appear before any failed initializer in their enum → reparse until macros stabilize (capped).
    // Only Count sentinels are injected — exporting every enumerator as a macro would collide with
    // later redefinitions of the same enum tag (see enum-merge fixture) and OOM ScannerInfo setup.
    // See https://github.com/eclipse-cdt/cdt/blob/main/core/org.eclipse.cdt.core/parser/org/eclipse/cdt/internal/core/dom/parser/ValueFactory.java
    //
    // DESNOTE(jbarber, 2026-07-20): Injecting `StatsAttack_Count` (etc.) as a ScannerInfo macro also
    // expands that identifier in its *defining* enum on reparse, which can drop or empty the typedef
    // from the final pass. Accumulate every pass and prefer richer bodies in mergeEnumParseResults
    // so Count sentinels remain available for array-bound lookup (`by_attack_counts[StatsAttack_Count]`).
    def parseAll(macrosForPass: Map[String, String]): List[(String, EnumParseResult)] = {
      val scannerInfo = CHeaderParser.scannerInfoFor(macrosForPass)
      corpus.files.map { file =>
        file.path.toString ->
          CHeaderParser.parseEnums(file.cdtSource, scannerInfo, alreadyStripped = true)
      }.toList
    }

    var macrosForPass = macros
    var results = parseAll(macrosForPass)
    val passResults = mutable.ListBuffer.empty[List[(String, EnumParseResult)]]
    passResults += results
    var pass = 0
    var continue = true
    while (continue && pass < 4) {
      pass += 1
      val warningCount = results.map(_._2.warnings.size).sum
      // Accumulate onto macrosForPass — never rebuild from the original `macros` alone, or a later
      // harvest that omits an earlier Count (because that enumerator name is now a macro in its
      // defining file) would drop dependencies needed by consumers.
      val nextMacros = macrosForPass ++ countSentinelMacros(results.map(_._2))
      if (nextMacros == macrosForPass || warningCount == 0) {
        continue = false
      } else {
        macrosForPass = nextMacros
        results = parseAll(macrosForPass)
        passResults += results
      }
    }
    // Per-pass merge keeps first-wins across files; across passes prefer richer bodies so a
    // Count-macro reparse that emptied a defining enum does not erase the earlier harvest.
    passResults.map(mergeEnumParseResults).reduceLeft(preferRicherEnumPass)
  }

  def countMacrosFromEnums(enums: Map[String, CEnumDefinition]): Map[String, String] =
    enums.values
      .flatMap(_.values)
      .foldLeft(Map.empty[String, String]) { (acc, value) =>
        if (
          acc.contains(value.name) ||
          !(value.name.endsWith("_Count") || value.name.endsWith("_SelfCount"))
        ) {
          acc
        } else {
          acc + (value.name -> value.value.toString)
        }
      }

  def enumeratorIntConstants(enums: Map[String, CEnumDefinition]): Map[String, Int] =
    // Post-hoc array-bound lookup table — not fed into ScannerInfo.
    enums.values
      .flatMap(_.values)
      .foldLeft(Map.empty[String, Int]) { (acc, value) =>
        if (acc.contains(value.name)) acc else acc + (value.name -> value.value)
      }

  private def countSentinelMacros(results: List[EnumParseResult]): Map[String, String] = {
    // DESNOTE(jbarber, 2026-07-20): Only export Count sentinels that appear before any failed
    // initializer in the same enum. Masterhand's ftMh_MS_Count is valid even when SelfCount fails
    // on a missing ftCo_MS_Count; Captain's Count is not, because an earlier initializer failed.
    val failedKeys = results
      .flatMap(_.warnings)
      .flatMap(warning => warning.split(':').headOption.map(_.trim).filter(_.nonEmpty))
      .toSet
    results
      .flatMap(_.enums.toList)
      .flatMap { case (enumName, definition) =>
        var seenFailure = false
        definition.values.flatMap { value =>
          if (failedKeys.contains(s"$enumName.${value.name}")) {
            seenFailure = true
          }
          if (
            !seenFailure && (value.name.endsWith("_Count") || value.name.endsWith("_SelfCount"))
          ) {
            Some(value.name -> value.value.toString)
          } else {
            None
          }
        }
      }
      .toMap
  }

  private def mergeEnumParseResults(
      results: List[(String, EnumParseResult)]
  ): EnumParseResult = {
    val warnings = mutable.ListBuffer.empty[String]
    warnings ++= results.flatMap(_._2.warnings)
    val conflictIgnored = mutable.LinkedHashMap.empty[String, mutable.ListBuffer[String]]
    val keptSource = mutable.LinkedHashMap.empty[String, String]
    val merged = mutable.LinkedHashMap.empty[String, CEnumDefinition]
    results.foreach { case (source, result) =>
      result.enums.foreach { case (name, definition) =>
        merged.get(name) match {
          case None =>
            merged(name) = definition
            keptSource(name) = source
          case Some(existing) if existing.values == definition.values =>
            ()
          case Some(existing) if existing.values.isEmpty && definition.values.nonEmpty =>
            merged(name) = definition
            keptSource(name) = source
          case Some(existing) if existing.values.nonEmpty && definition.values.isEmpty =>
            warnings += s"$name: Ignoring empty enum redefinition."
          case Some(_) =>
            conflictIgnored.getOrElseUpdate(name, mutable.ListBuffer.empty).append(source)
        }
      }
    }
    val conflicts = conflictIgnored.toList.map { case (name, ignored) =>
      NamedConflict(
        name = name,
        keptSource = keptSource.getOrElse(name, "<unknown>"),
        ignoredSources = ignored.toList.distinct
      )
    }
    warnings ++= CheadersNamedMerge.summarizeNameConflicts("enum", conflicts.map(_.name))
    EnumParseResult(merged.toMap, warnings.toList, conflicts)
  }

  private def preferRicherEnumPass(
      earlier: EnumParseResult,
      later: EnumParseResult
  ): EnumParseResult = {
    // Later passes resolve more Count-forward initializers; keep them as the baseline and only
    // restore from earlier when the Count-macro reparse dropped or emptied a defining enum.
    val merged = mutable.LinkedHashMap.from(later.enums)
    earlier.enums.foreach { case (name, definition) =>
      merged.get(name) match {
        case None =>
          merged(name) = definition
        case Some(existing) if existing.values.isEmpty && definition.values.nonEmpty =>
          merged(name) = definition
        case Some(existing) if definition.values.size > existing.values.size =>
          merged(name) = definition
        case Some(existing)
            if definition.values.size == existing.values.size &&
              hasCountSentinel(definition) && !hasCountSentinel(existing) =>
          merged(name) = definition
        case _ =>
          ()
      }
    }
    EnumParseResult(
      enums = merged.toMap,
      // Early-pass initializer failures are often fixed once Count macros exist; keep the later
      // pass's warnings (and conflicts) as the authoritative report.
      warnings = later.warnings,
      conflicts = later.conflicts
    )
  }

  private def hasCountSentinel(definition: CEnumDefinition): Boolean =
    definition.values.exists { v =>
      v.name.endsWith("_Count") || v.name.endsWith("_SelfCount")
    }
}
