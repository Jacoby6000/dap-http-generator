package io.github.jacoby6000.daphttp

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
