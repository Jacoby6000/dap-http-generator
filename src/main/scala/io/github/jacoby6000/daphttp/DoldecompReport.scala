package io.github.jacoby6000.daphttp

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.time.Instant
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter

/** Structured diagnostics collected while generating IR from C headers / symbols. */
final case class IrDiagnostics(
    codeSectionSkips: List[(String, List[String])] = Nil,
    unknownSectionSkips: List[(String, List[String])] = Nil,
    unresolvedSymbols: List[String] = Nil,
    missingTypes: List[(String, List[String])] = Nil,
    conflictingMacros: List[NamedConflict] = Nil,
    conflictingStructs: List[NamedConflict] = Nil,
    conflictingTypedefs: List[NamedConflict] = Nil,
    conflictingEnums: List[NamedConflict] = Nil,
    enumEvaluationWarnings: List[String] = Nil,
    includeHints: List[String] = Nil,
    otherWarnings: List[String] = Nil,
    headerRoots: List[String] = Nil,
    sourceFileCount: Int = 0,
    symbolCount: Int = 0,
    dataObjectCount: Int = 0,
    resolvedSymbolCount: Int = 0,
    operationCount: Int = 0
) {
  def isEmpty: Boolean =
    codeSectionSkips.isEmpty &&
      unknownSectionSkips.isEmpty &&
      unresolvedSymbols.isEmpty &&
      missingTypes.isEmpty &&
      conflictingMacros.isEmpty &&
      conflictingStructs.isEmpty &&
      conflictingTypedefs.isEmpty &&
      conflictingEnums.isEmpty &&
      enumEvaluationWarnings.isEmpty &&
      includeHints.isEmpty &&
      otherWarnings.isEmpty
}

final case class NamedConflict(
    name: String,
    keptSource: String,
    ignoredSources: List[String]
)

object DoldecompReport {
  private val TimestampFormat =
    DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'").withZone(ZoneOffset.UTC)

  def write(
      reportPath: Path,
      diagnostics: IrDiagnostics,
      summaryWarnings: List[String],
      symbolsPath: Option[Path] = None
  ): Path = {
    val parent = reportPath.getParent
    if (parent != null) {
      Files.createDirectories(parent)
    }
    val text = render(diagnostics, summaryWarnings, symbolsPath)
    Files.write(reportPath, text.getBytes(StandardCharsets.UTF_8))
    reportPath
  }

  def render(
      diagnostics: IrDiagnostics,
      summaryWarnings: List[String],
      symbolsPath: Option[Path] = None
  ): String = {
    val out = new StringBuilder
    out.append("# dap-http-generator cheaders report\n\n")
    out.append(s"- generatedAt: ${TimestampFormat.format(Instant.now())}\n")
    symbolsPath.foreach(path => out.append(s"- symbols: $path\n"))
    if (diagnostics.headerRoots.nonEmpty) {
      out.append(s"- headerRoots:\n")
      diagnostics.headerRoots.foreach(root => out.append(s"  - $root\n"))
    }
    out.append(s"- sourceFiles: ${diagnostics.sourceFileCount}\n")
    out.append(s"- symbols: ${diagnostics.symbolCount}\n")
    out.append(s"- dataObjectSymbols: ${diagnostics.dataObjectCount}\n")
    out.append(s"- resolvedSymbols: ${diagnostics.resolvedSymbolCount}\n")
    out.append(s"- operations: ${diagnostics.operationCount}\n")
    out.append("\n")

    out.append("## Summary warnings\n\n")
    if (summaryWarnings.isEmpty) {
      out.append("(none)\n\n")
    } else {
      summaryWarnings.foreach(warning => out.append(s"- $warning\n"))
      out.append("\n")
    }

    appendSectionSkips(out, "Code section skips", diagnostics.codeSectionSkips)
    appendSectionSkips(out, "Unknown section skips", diagnostics.unknownSectionSkips)

    out.append("## Unresolved symbols (no ctype / no matching C declaration)\n\n")
    if (diagnostics.unresolvedSymbols.isEmpty) {
      out.append("(none)\n\n")
    } else {
      out.append(s"count: ${diagnostics.unresolvedSymbols.size}\n\n")
      diagnostics.unresolvedSymbols.sorted.foreach(name => out.append(s"- $name\n"))
      out.append("\n")
    }

    out.append("## Missing type definitions\n\n")
    if (diagnostics.missingTypes.isEmpty) {
      out.append("(none)\n\n")
    } else {
      diagnostics.missingTypes.foreach { case (typeName, symbols) =>
        out.append(s"### `$typeName` (${symbols.size})\n\n")
        symbols.sorted.foreach(name => out.append(s"- $name\n"))
        out.append("\n")
      }
    }

    appendConflicts(out, "Conflicting macros", diagnostics.conflictingMacros)
    appendConflicts(out, "Conflicting structs", diagnostics.conflictingStructs)
    appendConflicts(out, "Conflicting typedefs", diagnostics.conflictingTypedefs)
    appendConflicts(out, "Conflicting enums", diagnostics.conflictingEnums)

    out.append("## Enum evaluation warnings\n\n")
    if (diagnostics.enumEvaluationWarnings.isEmpty) {
      out.append("(none)\n\n")
    } else {
      diagnostics.enumEvaluationWarnings.foreach(warning => out.append(s"- $warning\n"))
      out.append("\n")
    }

    out.append("## Include path hints\n\n")
    if (diagnostics.includeHints.isEmpty) {
      out.append("(none)\n\n")
    } else {
      diagnostics.includeHints.foreach(hint => out.append(s"- $hint\n"))
      out.append("\n")
    }

    out.append("## Other warnings\n\n")
    if (diagnostics.otherWarnings.isEmpty) {
      out.append("(none)\n\n")
    } else {
      diagnostics.otherWarnings.foreach(warning => out.append(s"- $warning\n"))
      out.append("\n")
    }

    out.toString
  }

  private def appendSectionSkips(
      out: StringBuilder,
      title: String,
      skips: List[(String, List[String])]
  ): Unit = {
    out.append(s"## $title\n\n")
    if (skips.isEmpty) {
      out.append("(none)\n\n")
    } else {
      skips.foreach { case (section, names) =>
        out.append(s"### `$section` (${names.size})\n\n")
        names.sorted.foreach(name => out.append(s"- $name\n"))
        out.append("\n")
      }
    }
  }

  private def appendConflicts(
      out: StringBuilder,
      title: String,
      conflicts: List[NamedConflict]
  ): Unit = {
    out.append(s"## $title\n\n")
    if (conflicts.isEmpty) {
      out.append("(none)\n\n")
    } else {
      out.append(s"count: ${conflicts.size}\n\n")
      conflicts.sortBy(_.name).foreach { conflict =>
        out.append(s"### `${conflict.name}`\n\n")
        out.append(s"- kept: ${conflict.keptSource}\n")
        conflict.ignoredSources.foreach(source => out.append(s"- ignored: $source\n"))
        out.append("\n")
      }
    }
  }
}
