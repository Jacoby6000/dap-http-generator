package io.github.jacoby6000.daphttp

import org.eclipse.cdt.core.dom.ast.IASTCompositeTypeSpecifier

import scala.collection.mutable

private[daphttp] final case class TypeCorpus(
    structs: MergedNamedMap[IASTCompositeTypeSpecifier],
    typedefs: MergedNamedMap[String],
    globals: Map[String, GlobalVariableDeclaration],
    fieldInitializerLengths: Map[(String, String), Int]
)

private[daphttp] object CheadersTypeCorpus {

  def loadAllMacros(corpus: HeaderCorpus): MergedNamedMap[String] = {
    val entries = corpus.files.flatMap { file =>
      CHeaderParser
        .extractMacros(file.cdtSource, alreadyStripped = true)
        .toList
        .map { case (name, value) => (name, value, file.path.toString) }
    }.toList
    CheadersNamedMerge.mergeNamedEntries(entries, "macro", (a: String, b: String) => a == b)
  }

  def load(
      corpus: HeaderCorpus,
      macros: Map[String, String],
      arrayConstants: Map[String, Int]
  ): TypeCorpus = {
    // DESNOTE(jbarber, 2026-07-20): .c sources are neutralized (no huge initializers / bodies) before
    // CDT, so retaining struct ASTs from them is safe; skipping .c structs broke fixtures that define
    // types in .c and would miss real TU-local types.
    val scannerInfo = CHeaderParser.scannerInfoFor(macros)
    val structEntries =
      mutable.ListBuffer.empty[(String, IASTCompositeTypeSpecifier, String)]
    val typedefEntries = mutable.ListBuffer.empty[(String, String, String)]
    val globalEntries = mutable.ListBuffer.empty[GlobalVariableDeclaration]
    corpus.files.foreach { file =>
      val parsed =
        CHeaderParser.parseDeclarations(
          file.cdtSource,
          scannerInfo,
          arrayConstants,
          alreadyStripped = true
        )
      val source = file.path.toString
      structEntries ++= parsed.structs.map { case (name, spec) => (name, spec, source) }
      typedefEntries ++= parsed.typedefs.map { case (name, target) => (name, target, source) }
      globalEntries ++= parsed.globals
    }
    val structs =
      CheadersNamedMerge.mergeNamedEntries(
        structEntries.toList,
        "struct",
        structDefinitionsEquivalent
      )
    val typedefs =
      CheadersNamedMerge.mergeNamedEntries(
        typedefEntries.toList,
        "typedef",
        (a: String, b: String) => a == b
      )
    val globals = globalEntries.toList
      .groupBy(_.name)
      .view
      .mapValues(mergeGlobalDeclarations)
      .toMap
    // Field-initializer lengths need real initializer ASTs. Only re-parse small .c files without
    // neutralization so Melee data objects (often MBs) are skipped.
    val maxFieldInitBytes = 64 * 1024
    val fieldInitScanner = CHeaderParser.scannerInfoFor(macros)
    val fieldInitializerLengths = corpus.files.iterator
      .filter(file => file.isCSource && file.raw.length <= maxFieldInitBytes)
      .flatMap { file =>
        val forInits = CHeaderParser.stripComments(file.raw)
        CHeaderParser
          .parseStructFieldInitializerLengths(
            forInits,
            structs.values,
            fieldInitScanner,
            alreadyStripped = true
          )
          .toList
      }
      .toList
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).max)
      .toMap
    TypeCorpus(structs, typedefs, globals, fieldInitializerLengths)
  }

  def mergeGlobalDeclarations(
      declarations: List[GlobalVariableDeclaration]
  ): GlobalVariableDeclaration = {
    // DESNOTE(jbarber, 2026-07-19): Files.walk order is not stable across environments, so never
    // take "first declaration wins" for lengths/pointer depth. Prefer non-static definitions with
    // explicit array metadata; array-ness and pointerDepth come from that primary so a mismatched
    // forward declaration cannot change the preferred definition's route shape.
    def preference(d: GlobalVariableDeclaration): (Boolean, Boolean, Boolean, Boolean, String) =
      (
        !d.isStatic,
        d.declaratorLength.isDefined,
        d.initializerLength.isDefined,
        d.typeName.nonEmpty,
        d.typeName
      )
    val ordered = declarations.sortBy(preference)(
      Ordering
        .Tuple5(
          Ordering.Boolean,
          Ordering.Boolean,
          Ordering.Boolean,
          Ordering.Boolean,
          Ordering.String
        )
        .reverse
    )
    val primary = ordered.head
    val compatibleArrayDeclarations =
      if (primary.isArray) ordered.filter(d => d.isArray && d.pointerDepth == primary.pointerDepth)
      else Nil
    GlobalVariableDeclaration(
      name = primary.name,
      typeName = ordered.map(_.typeName).find(_.nonEmpty).getOrElse(primary.typeName),
      isArray = primary.isArray,
      declaratorLength = compatibleArrayDeclarations.flatMap(_.declaratorLength).headOption,
      initializerLength = compatibleArrayDeclarations.flatMap(_.initializerLength).headOption,
      pointerDepth = primary.pointerDepth,
      isStatic = primary.isStatic
    )
  }

  private def structDefinitionsEquivalent(
      left: IASTCompositeTypeSpecifier,
      right: IASTCompositeTypeSpecifier
  ): Boolean =
    structFieldSignature(left) == structFieldSignature(right)

  private def structFieldSignature(composite: IASTCompositeTypeSpecifier): List[String] =
    CHeaderParser.extractFields(composite).map { field =>
      val fieldName =
        Option(field.declarator.getName).map(_.toString).getOrElse("")
      s"${field.typeName}|$fieldName|${field.bitFieldWidth}|${field.unionGroup}|${field.offsetBytes}"
    }
}
