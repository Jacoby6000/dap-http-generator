package io.github.jacoby6000.daphttp

import org.eclipse.cdt.core.dom.ast.IASTArrayDeclarator
import org.eclipse.cdt.core.dom.ast.IASTCompositeTypeSpecifier
import org.eclipse.cdt.core.dom.ast.IASTDeclSpecifier
import org.eclipse.cdt.core.dom.ast.IASTDeclaration
import org.eclipse.cdt.core.dom.ast.IASTDeclarator
import org.eclipse.cdt.core.dom.ast.IASTEnumerationSpecifier
import org.eclipse.cdt.core.dom.ast.IASTEqualsInitializer
import org.eclipse.cdt.core.dom.ast.IASTFieldDeclarator
import org.eclipse.cdt.core.dom.ast.IASTFunctionDeclarator
import org.eclipse.cdt.core.dom.ast.IASTInitializer
import org.eclipse.cdt.core.dom.ast.IASTInitializerClause
import org.eclipse.cdt.core.dom.ast.IASTInitializerList
import org.eclipse.cdt.core.dom.ast.IASTLiteralExpression
import org.eclipse.cdt.core.dom.ast.IASTSimpleDeclaration
import org.eclipse.cdt.core.dom.ast.IASTStandardFunctionDeclarator
import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit
import org.eclipse.cdt.core.dom.ast.gnu.c.GCCLanguage
import org.eclipse.cdt.core.model.ILanguage
import org.eclipse.cdt.core.parser.DefaultLogService
import org.eclipse.cdt.core.parser.FileContent
import org.eclipse.cdt.core.parser.IncludeFileContentProvider
import org.eclipse.cdt.core.parser.ScannerInfo
import org.eclipse.cdt.internal.core.dom.parser.ValueFactory

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

final case class GlobalVariableDeclaration(
    name: String,
    typeName: String,
    isArray: Boolean,
    declaratorLength: Option[Int],
    initializerLength: Option[Int],
    pointerDepth: Int,
    isStatic: Boolean = false
) {
  def resolvedArrayLength: Option[Int] = declaratorLength.orElse(initializerLength)
}

final case class StructFieldDecl(
    typeName: String,
    declarator: IASTDeclarator,
    unionGroup: Option[String],
    bitFieldWidth: Option[Int] = None,
    offsetBytes: Option[Int] = None
)

final case class FunctionPointerSignature(
    name: String,
    params: List[FunctionPointerParam],
    returnType: String
)

final case class CEnumDefinition(
    name: String,
    values: List[IrEnumValue]
)

final case class EnumParseResult(
    enums: Map[String, CEnumDefinition],
    warnings: List[String] = Nil,
    conflicts: List[NamedConflict] = Nil
)

/** One CDT translation unit extracted into the declaration kinds we care about. */
final case class ParsedDeclarations(
    structs: List[(String, IASTCompositeTypeSpecifier)],
    typedefs: List[(String, String)],
    globals: List[GlobalVariableDeclaration]
)

object CHeaderParser {
  def parse(
      headerSource: String,
      extraMacros: Map[String, String] = Map.empty
  ): List[(String, IASTCompositeTypeSpecifier)] =
    parseDeclarations(headerSource, extraMacros).structs

  def parseTypedefs(
      source: String,
      extraMacros: Map[String, String] = Map.empty
  ): Map[String, String] =
    parseDeclarations(source, extraMacros).typedefs.toMap

  def parseEnums(
      source: String,
      extraMacros: Map[String, String] = Map.empty
  ): EnumParseResult =
    parseEnums(source, scannerInfoFor(extraMacros), alreadyStripped = false)

  private[daphttp] def parseEnums(
      source: String,
      scannerInfo: ScannerInfo,
      alreadyStripped: Boolean
  ): EnumParseResult = {
    // DESNOTE(jbarber, 2026-07-19): Pass macros through CDT ScannerInfo so the preprocessor
    // expands #define uses in enumerator initializers before we evaluate them. Prefer CDT's
    // ValueFactory over hand-rolled expression/macro evaluation.
    // See https://github.com/eclipse-cdt/cdt/blob/main/core/org.eclipse.cdt.core/parser/org/eclipse/cdt/internal/core/dom/parser/ValueFactory.java
    parseTranslationUnit(source, "header.h", scannerInfo, alreadyStripped)
      .map { translationUnit =>
        val warnings = ListBuffer.empty[String]
        val enums = translationUnit.getDeclarations.toList
          .flatMap(extractEnumDefinitions(_, warnings))
          .toMap
        EnumParseResult(enums, warnings.toList)
      }
      .getOrElse(EnumParseResult(Map.empty, Nil))
  }

  private[daphttp] def extractMacros(source: String): Map[String, String] =
    extractMacros(source, alreadyStripped = false)

  private[daphttp] def extractMacros(
      source: String,
      alreadyStripped: Boolean
  ): Map[String, String] = {
    // DESNOTE(jbarber, 2026-07-19): Always collect macros from CDT's preprocessor AST — never
    // regex/#define line scraping. Expansions (including parenthesized bodies) feed ScannerInfo
    // for later translation units.
    parseTranslationUnit(source, "macros.h", EmptyScannerInfo, alreadyStripped)
      .map { translationUnit =>
        translationUnit.getMacroDefinitions.toList.flatMap { macroDef =>
          val name = Option(macroDef.getName).map(_.toString.trim).filter(_.nonEmpty)
          val expansion = Option(macroDef.getExpansion).map(_.trim).filter(_.nonEmpty)
          (name, expansion) match {
            case (Some(n), Some(e)) => Some(n -> e)
            case _                  => None
          }
        }.toMap
      }
      .getOrElse(Map.empty)
  }

  def parseGlobalDeclarations(
      source: String,
      extraMacros: Map[String, String] = Map.empty,
      arrayConstants: Map[String, Int] = Map.empty
  ): List[GlobalVariableDeclaration] =
    parseDeclarations(source, extraMacros, arrayConstants).globals

  def parseDeclarations(
      source: String,
      extraMacros: Map[String, String] = Map.empty,
      arrayConstants: Map[String, Int] = Map.empty
  ): ParsedDeclarations =
    parseDeclarations(source, scannerInfoFor(extraMacros), arrayConstants, alreadyStripped = false)

  private[daphttp] def parseDeclarations(
      source: String,
      scannerInfo: ScannerInfo,
      arrayConstants: Map[String, Int],
      alreadyStripped: Boolean
  ): ParsedDeclarations =
    parseTranslationUnit(source, "header.h", scannerInfo, alreadyStripped)
      .map { translationUnit =>
        val declarations = translationUnit.getDeclarations.toList
        ParsedDeclarations(
          structs = declarations.flatMap(extractStructs),
          typedefs = declarations.flatMap(extractTypedef),
          globals = declarations.flatMap(extractGlobalDeclaration(_, arrayConstants))
        )
      }
      .getOrElse(ParsedDeclarations(Nil, Nil, Nil))

  def parseStructFieldInitializerLengths(
      source: String,
      structs: Map[String, IASTCompositeTypeSpecifier],
      extraMacros: Map[String, String] = Map.empty
  ): Map[(String, String), Int] =
    parseStructFieldInitializerLengths(
      source,
      structs,
      scannerInfoFor(extraMacros),
      alreadyStripped = false
    )

  private[daphttp] def parseStructFieldInitializerLengths(
      source: String,
      structs: Map[String, IASTCompositeTypeSpecifier],
      scannerInfo: ScannerInfo,
      alreadyStripped: Boolean
  ): Map[(String, String), Int] =
    parseTranslationUnit(source, "source.c", scannerInfo, alreadyStripped)
      .map { translationUnit =>
        translationUnit.getDeclarations.toList.flatMap { declaration =>
          extractGlobalDeclaration(declaration).flatMap { global =>
            structs.get(global.typeName).toList.flatMap { struct =>
              extractGlobalDeclaratorWithInitializer(declaration, global.typeName).toList.flatMap {
                declarator =>
                  extractFieldInitializerLengths(struct, global.typeName, declarator).toList
              }
            }
          }
        }.toMap
      }
      .getOrElse(Map.empty)

  private val BuiltInMacros: Map[String, String] = Map(
    "UNK_T" -> "void*",
    "UNK_RET" -> "void",
    "UNK_PARAMS" -> "void"
  )

  /** Opaque / placeholder macros always available during CDT parses and type validation. */
  private[daphttp] def builtInMacros: Map[String, String] = BuiltInMacros

  private val EmptyScannerInfo: ScannerInfo = scannerInfoFor(Map.empty)

  private[daphttp] def scannerInfoFor(extraMacros: Map[String, String]): ScannerInfo =
    new ScannerInfo((BuiltInMacros ++ extraMacros).asJava, Array.empty[String])

  private[daphttp] def stripComments(content: String): String =
    content
      .replaceAll("(?s)/\\*.*?\\*/", "")
      .replaceAll("(?m)//.*$", "")

  // DESNOTE(jbarber, 2026-07-20): Melee .c files carry huge static aggregate / string initializers
  // and large function bodies. CDT still lexes/parses those at TU scope even with
  // OPTION_SKIP_FUNCTION_BODIES for the declaration shape we need, which OOMs on full-tree scans.
  // Neutralize heavy top-level payloads before CDT; keep declaration syntax (including `[]` bounds).
  // When the declarator is an unsized array (`[]`), rewrite the bound from the initializer count so
  // `type name[] = {…}` / `= "…"` still yields a length after the payload is dropped.
  // See https://github.com/eclipse-cdt/cdt/blob/main/core/org.eclipse.cdt.core/model/org/eclipse/cdt/core/model/ILanguage.java
  private[daphttp] def neutralizeHeavyTopLevelContent(source: String): String = {
    val chars = source.toCharArray
    val out = new java.lang.StringBuilder(chars.length)
    var i = 0
    var depth = 0
    var inString = false
    var inChar = false

    def lastSignificantChar(): Option[Char] = {
      var j = out.length() - 1
      while (j >= 0 && Character.isWhitespace(out.charAt(j))) j -= 1
      if (j < 0) None else Some(out.charAt(j))
    }

    def emptyArrayBracketStart(): Option[Int] = {
      var j = out.length() - 1
      while (j >= 0 && Character.isWhitespace(out.charAt(j))) j -= 1
      if (j < 1 || out.charAt(j) != ']') None
      else {
        var k = j - 1
        while (k >= 0 && Character.isWhitespace(out.charAt(k))) k -= 1
        if (k >= 0 && out.charAt(k) == '[') Some(k) else None
      }
    }

    def rewriteEmptyArrayBound(openBracket: Int, length: Int): Unit = {
      var end = openBracket + 1
      while (end < out.length() && Character.isWhitespace(out.charAt(end))) end += 1
      if (end < out.length() && out.charAt(end) == ']') {
        val _ = out.replace(openBracket, end + 1, s"[$length]")
      }
    }

    def skipBalancedBrace(openIndex: Int): (Int, Int) = {
      var j = openIndex
      var d = 0
      var s = false
      var ch = false
      var elements = 0
      var elementStarted = false
      while (j < chars.length) {
        val c = chars(j)
        if (s) {
          if (c == '\\' && j + 1 < chars.length) j += 2
          else {
            if (c == '"') s = false
            j += 1
          }
        } else if (ch) {
          if (c == '\\' && j + 1 < chars.length) j += 2
          else {
            if (c == '\'') ch = false
            j += 1
          }
        } else if (c == '"') {
          s = true
          if (d == 1) elementStarted = true
          j += 1
        } else if (c == '\'') {
          ch = true
          if (d == 1) elementStarted = true
          j += 1
        } else if (c == '{') {
          if (d == 1) elementStarted = true
          d += 1
          j += 1
        } else if (c == '}') {
          if (d == 1 && elementStarted) {
            elements += 1
            elementStarted = false
          }
          d -= 1
          j += 1
          if (d == 0) return (j, elements)
        } else if (c == ',' && d == 1) {
          if (elementStarted) {
            elements += 1
            elementStarted = false
          }
          j += 1
        } else {
          if (d == 1 && !Character.isWhitespace(c)) elementStarted = true
          j += 1
        }
      }
      (chars.length, elements)
    }

    def skipStringLiteral(openIndex: Int): (Int, Int) = {
      var j = openIndex + 1
      var len = 0
      while (j < chars.length) {
        val c = chars(j)
        if (c == '\\' && j + 1 < chars.length) {
          len += 1
          j += 2
        } else if (c == '"') {
          return (j + 1, len + 1) // include NUL for C string storage
        } else {
          len += 1
          j += 1
        }
      }
      (chars.length, len + 1)
    }

    while (i < chars.length) {
      val c = chars(i)
      if (inString) {
        out.append(c)
        if (c == '\\' && i + 1 < chars.length) {
          out.append(chars(i + 1))
          i += 2
        } else {
          if (c == '"') inString = false
          i += 1
        }
      } else if (inChar) {
        out.append(c)
        if (c == '\\' && i + 1 < chars.length) {
          out.append(chars(i + 1))
          i += 2
        } else {
          if (c == '\'') inChar = false
          i += 1
        }
      } else if (c == '"') {
        inString = true
        out.append(c)
        i += 1
      } else if (c == '\'') {
        inChar = true
        out.append(c)
        i += 1
      } else if (c == '{') {
        if (depth == 0 && lastSignificantChar().contains(')')) {
          i = skipBalancedBrace(i)._1
          out.append('{').append('}')
        } else {
          out.append(c)
          depth += 1
          i += 1
        }
      } else if (c == '}') {
        out.append(c)
        if (depth > 0) depth -= 1
        i += 1
      } else if (c == '=' && depth == 0) {
        val emptyArrayAt = emptyArrayBracketStart()
        i += 1
        while (i < chars.length && Character.isWhitespace(chars(i))) i += 1
        if (i < chars.length && chars(i) == '{') {
          val (next, count) = skipBalancedBrace(i)
          i = next
          emptyArrayAt match {
            case Some(open) if count > 0 =>
              rewriteEmptyArrayBound(open, count)
            case _ =>
              out.append('=').append('{').append('}')
          }
        } else if (i < chars.length && chars(i) == '"') {
          val (next, count) = skipStringLiteral(i)
          i = next
          emptyArrayAt match {
            case Some(open) if count > 0 =>
              rewriteEmptyArrayBound(open, count)
            case _ =>
              out.append('=').append('"').append('"')
          }
        } else {
          out.append('=')
          // Scalar / other initializer — keep as-is from current i.
        }
      } else {
        out.append(c)
        i += 1
      }
    }
    out.toString
  }

  private[daphttp] def prepareCdtSource(source: String, neutralizeHeavyContent: Boolean): String = {
    val noComments = stripComments(source)
    if (neutralizeHeavyContent) neutralizeHeavyTopLevelContent(noComments) else noComments
  }

  private def parseTranslationUnit(
      source: String,
      fileName: String,
      scannerInfo: ScannerInfo,
      alreadyStripped: Boolean
  ): Option[IASTTranslationUnit] = {
    val stripped = if (alreadyStripped) source else stripComments(source)
    try {
      Some(
        GCCLanguage.getDefault.getASTTranslationUnit(
          FileContent.create(fileName, stripped.toCharArray),
          scannerInfo,
          IncludeFileContentProvider.getEmptyFilesProvider,
          null,
          ILanguage.OPTION_SKIP_FUNCTION_BODIES,
          new DefaultLogService()
        )
      )
    } catch {
      case NonFatal(_) => None
    }
  }

  private[daphttp] def extractGlobalDeclaration(
      declaration: IASTDeclaration,
      arrayConstants: Map[String, Int] = Map.empty
  ): List[GlobalVariableDeclaration] =
    declaration match {
      case simple: IASTSimpleDeclaration =>
        simple.getDeclSpecifier match {
          case composite: IASTCompositeTypeSpecifier =>
            // Skip typedef struct Foo { ... } Foo; — those declarators are type aliases, not globals.
            // Keep `struct Tag { ... } symbol;` so declaration lookup can resolve the symbol.
            if (simple.getDeclSpecifier.getStorageClass == IASTDeclSpecifier.sc_typedef) {
              Nil
            } else {
              val typeName =
                normalizeTypeName(Option(composite.getName).map(_.toString).getOrElse(""))
              if (typeName.isEmpty) Nil
              else extractGlobalDeclarators(typeName, simple, arrayConstants)
            }
          case _ =>
            val typeName = normalizeTypeName(simple.getDeclSpecifier.getRawSignature)
            if (typeName.isEmpty) Nil
            else extractGlobalDeclarators(typeName, simple, arrayConstants)
        }
      case _ =>
        Nil
    }

  private[daphttp] def extractGlobalDeclarators(
      typeName: String,
      simple: IASTSimpleDeclaration,
      arrayConstants: Map[String, Int] = Map.empty
  ): List[GlobalVariableDeclaration] =
    simple.getDeclarators.toList.flatMap { declarator =>
      val name = extractDeclaratorName(declarator)
      if (name.isEmpty || isFunctionDeclarator(declarator)) {
        Nil
      } else {
        val isArray = isArrayDeclarator(declarator)
        val declaratorLength = if (isArray) arrayLength(declarator, arrayConstants) else None
        val initializerLength = if (isArray) initializerElementCount(declarator) else None
        List(
          GlobalVariableDeclaration(
            name = name,
            typeName = typeName,
            isArray = isArray,
            declaratorLength = declaratorLength,
            initializerLength = initializerLength,
            pointerDepth = pointerDepth(declarator),
            isStatic = simple.getDeclSpecifier.getStorageClass == IASTDeclSpecifier.sc_static
          )
        )
      }
    }

  private def isFunctionDeclarator(declarator: IASTDeclarator): Boolean =
    declaratorChain(declarator).exists(_.isInstanceOf[IASTFunctionDeclarator])

  def isFunctionPointer(declarator: IASTDeclarator): Boolean =
    isFunctionDeclarator(declarator)

  def extractFunctionPointerSignature(
      declarator: IASTDeclarator,
      typeName: String
  ): Option[FunctionPointerSignature] = {
    val funcDeclOpt = declaratorChain(declarator).collectFirst {
      case funcDecl: IASTStandardFunctionDeclarator => funcDecl
    }
    funcDeclOpt.map { funcDecl =>
      val name = extractDeclaratorName(declarator)
      val returnType = normalizeTypeName(typeName)
      val paramList = Option(funcDecl.getParameters).toList.flatMap(_.toList)
      val params = paramList.zipWithIndex
        .map { case (param, idx) =>
          val paramType = normalizeTypeName(param.getDeclSpecifier.getRawSignature)
          val paramName = Option(param.getDeclarator)
            .map(extractDeclaratorName)
            .filter(_.nonEmpty)
            .getOrElse(s"arg$idx")
          FunctionPointerParam(paramType, paramName)
        }
        .filterNot(_.typeName == "void")
      FunctionPointerSignature(name, params, returnType)
    }
  }

  private def isArrayDeclarator(declarator: IASTDeclarator): Boolean =
    declaratorChain(declarator).exists(_.isInstanceOf[IASTArrayDeclarator])

  def isArrayField(declarator: IASTDeclarator): Boolean = isArrayDeclarator(declarator)

  private def extractStructs(
      declaration: IASTDeclaration
  ): List[(String, IASTCompositeTypeSpecifier)] = {
    declaration match {
      case simple: IASTSimpleDeclaration =>
        simple.getDeclSpecifier match {
          case composite: IASTCompositeTypeSpecifier
              if composite.getKey == IASTCompositeTypeSpecifier.k_struct || composite.getKey == IASTCompositeTypeSpecifier.k_union =>
            val aliases = simple.getDeclarators.toList
              .map(extractDeclaratorName)
              .filter(_.nonEmpty)
            val tagName =
              Option(composite.getName).map(_.toString.trim).filter(_.nonEmpty)
            val self = (aliases ++ tagName.toList).distinct.map(_ -> composite)
            self ++ extractNestedStructs(composite)
          case _ =>
            Nil
        }
      case _ =>
        Nil
    }
  }

  private def extractNestedStructs(
      composite: IASTCompositeTypeSpecifier
  ): List[(String, IASTCompositeTypeSpecifier)] =
    composite.getMembers.toList.flatMap {
      case member: IASTSimpleDeclaration =>
        member.getDeclSpecifier match {
          case nested: IASTCompositeTypeSpecifier
              if nested.getKey == IASTCompositeTypeSpecifier.k_struct || nested.getKey == IASTCompositeTypeSpecifier.k_union =>
            val tagName =
              Option(nested.getName).map(_.toString.trim).filter(_.nonEmpty)
            tagName.toList.map(_ -> nested) ++ extractNestedStructs(nested)
          case _ =>
            Nil
        }
      case _ =>
        Nil
    }

  private def extractEnumDefinitions(
      declaration: IASTDeclaration,
      warnings: ListBuffer[String]
  ): List[(String, CEnumDefinition)] =
    declaration match {
      case simple: IASTSimpleDeclaration =>
        simple.getDeclSpecifier match {
          case enumSpec: IASTEnumerationSpecifier =>
            enumDefinitionEntries(simple, enumSpec, warnings)
          case composite: IASTCompositeTypeSpecifier =>
            composite.getMembers.toList.flatMap {
              case nested: IASTSimpleDeclaration =>
                extractEnumDefinitions(nested, warnings)
              case _ =>
                Nil
            }
          case _ =>
            Nil
        }
      case _ =>
        Nil
    }

  private def enumDefinitionEntries(
      simple: IASTSimpleDeclaration,
      enumSpec: IASTEnumerationSpecifier,
      warnings: ListBuffer[String]
  ): List[(String, CEnumDefinition)] = {
    if (enumSpec.getEnumerators.isEmpty) {
      Nil
    } else {
      val names = enumerationTypeNames(simple, enumSpec)
      if (names.isEmpty) {
        Nil
      } else {
        val primaryName = names.head
        val values = extractEnumeratorValues(primaryName, enumSpec, warnings)
        val definition = CEnumDefinition(primaryName, values)
        names.map(_ -> definition)
      }
    }
  }

  private def enumerationTypeNames(
      simple: IASTSimpleDeclaration,
      enumSpec: IASTEnumerationSpecifier
  ): List[String] = {
    val tagName = Option(enumSpec.getName).map(_.toString.trim).filter(_.nonEmpty)
    val aliases = simple.getDeclarators.toList
      .map(extractDeclaratorName)
      .filter(_.nonEmpty)
    val isTypedef =
      simple.getDeclSpecifier.getStorageClass == IASTDeclSpecifier.sc_typedef
    if (tagName.nonEmpty) {
      (tagName.toList ++ (if (isTypedef) aliases else Nil)).distinct
    } else if (isTypedef) {
      aliases.distinct
    } else {
      aliases.headOption.map(name => s"${toPascalCaseIdentifier(name)}Enum").toList
    }
  }

  private def toPascalCaseIdentifier(raw: String): String = {
    raw
      .split("[_\\s]+")
      .filter(_.nonEmpty)
      .map(part => part.take(1).toUpperCase + part.drop(1))
      .mkString
  }

  private def extractEnumeratorValues(
      enumName: String,
      enumSpec: IASTEnumerationSpecifier,
      warnings: ListBuffer[String]
  ): List[IrEnumValue] = {
    val known = scala.collection.mutable.LinkedHashMap.empty[String, Int]
    var nextValue = 0
    enumSpec.getEnumerators.toList.foreach { enumerator =>
      val name =
        Option(enumerator.getName).map(_.toString.trim).filter(_.nonEmpty).getOrElse("")
      if (name.nonEmpty) {
        val value = Option(enumerator.getValue) match {
          case None =>
            nextValue
          case Some(expr) =>
            Option(ValueFactory.getConstantNumericalValue(expr))
              .map(_.intValue())
              .getOrElse {
                // DESNOTE(jbarber, 2026-07-19): Never silently invent a sequential value for an
                // explicit initializer we failed to evaluate — warn so wrong layouts are visible.
                warnings +=
                  s"$enumName.$name: Unable to evaluate enumerator initializer '${expr.getRawSignature}'; falling back to sequential value $nextValue."
                nextValue
              }
        }
        known(name) = value
        nextValue = value + 1
      }
    }
    known.toList.map { case (name, value) => IrEnumValue(name, value) }
  }

  private def extractTypedef(
      declaration: IASTDeclaration
  ): List[(String, String)] = {
    declaration match {
      case simple: IASTSimpleDeclaration
          if simple.getDeclSpecifier.getStorageClass == IASTDeclSpecifier.sc_typedef =>
        simple.getDeclSpecifier match {
          case _: IASTCompositeTypeSpecifier =>
            Nil
          case enumSpec: IASTEnumerationSpecifier =>
            val tagName =
              Option(enumSpec.getName).map(_.toString.trim).filter(_.nonEmpty)
            tagName.toList.flatMap { tag =>
              simple.getDeclarators.toList.flatMap { declarator =>
                val name = extractDeclaratorName(declarator)
                if (name.isEmpty || name == tag || isFunctionDeclarator(declarator)) Nil
                else {
                  val pointerPart = (0 until pointerDepth(declarator)).map(_ => "*").mkString
                  List(name -> s"$tag$pointerPart")
                }
              }
            }
          case spec =>
            val baseType = normalizeTypeName(spec.getRawSignature)
            simple.getDeclarators.toList.flatMap { declarator =>
              val name = extractDeclaratorName(declarator)
              if (name.isEmpty) Nil
              else if (isFunctionDeclarator(declarator)) {
                List(name -> "void*")
              } else {
                val pointerPart = (0 until pointerDepth(declarator)).map(_ => "*").mkString
                List(name -> s"$baseType$pointerPart")
              }
            }
        }
      case _ => Nil
    }
  }

  def extractFields(composite: IASTCompositeTypeSpecifier): List[StructFieldDecl] = {
    var unionCounter = 0
    composite.getMembers.toList.flatMap {
      case member: IASTSimpleDeclaration =>
        member.getDeclSpecifier match {
          case unionSpec: IASTCompositeTypeSpecifier
              if unionSpec.getKey == IASTCompositeTypeSpecifier.k_union =>
            val unionGroup = Some(s"union${unionCounter}")
            unionCounter += 1
            unionSpec.getMembers.toList.flatMap {
              case unionMember: IASTSimpleDeclaration =>
                val baseType = normalizeTypeName(unionMember.getDeclSpecifier.getRawSignature)
                unionMember.getDeclarators.toList.flatMap { declarator =>
                  Option.when(extractDeclaratorName(declarator).nonEmpty) {
                    StructFieldDecl(baseType, declarator, unionGroup, bitFieldWidth(declarator))
                  }
                }
              case _ => Nil
            }
          case structSpec: IASTCompositeTypeSpecifier
              if structSpec.getKey == IASTCompositeTypeSpecifier.k_struct =>
            val baseType = Option(structSpec.getName)
              .map(_.toString.trim)
              .filter(_.nonEmpty)
              .getOrElse(normalizeTypeName(member.getDeclSpecifier.getRawSignature))
            member.getDeclarators.toList.flatMap { declarator =>
              Option.when(extractDeclaratorName(declarator).nonEmpty) {
                StructFieldDecl(baseType, declarator, unionGroup = None, bitFieldWidth(declarator))
              }
            }
          case enumSpec: IASTEnumerationSpecifier =>
            member.getDeclarators.toList.flatMap { declarator =>
              Option.when(extractDeclaratorName(declarator).nonEmpty) {
                val typeName = enumerationTypeNames(member, enumSpec).headOption.getOrElse {
                  normalizeTypeName(enumSpec.getRawSignature)
                }
                StructFieldDecl(typeName, declarator, unionGroup = None, bitFieldWidth(declarator))
              }
            }
          case _ =>
            val baseType = normalizeTypeName(member.getDeclSpecifier.getRawSignature)
            member.getDeclarators.toList.flatMap { declarator =>
              Option.when(extractDeclaratorName(declarator).nonEmpty) {
                StructFieldDecl(baseType, declarator, unionGroup = None, bitFieldWidth(declarator))
              }
            }
        }
      case _ => Nil
    }
  }

  def bitFieldWidth(declarator: IASTDeclarator): Option[Int] =
    declaratorChain(declarator).collectFirst(Function.unlift {
      case field: IASTFieldDeclarator =>
        Option(field.getBitFieldSize).flatMap { expr =>
          Option(ValueFactory.getConstantNumericalValue(expr)).map(_.intValue())
        }
      case _ =>
        None
    })

  def pointerDepth(declarator: IASTDeclarator): Int = {
    declaratorChain(declarator).map(_.getPointerOperators.length).sum
  }

  def arrayLength(
      declarator: IASTDeclarator,
      constantLookup: Map[String, Int] = Map.empty
  ): Option[Int] = {
    // DESNOTE(jbarber, 2026-07-19): Prefer CDT ValueFactory on the preprocessed expression.
    // When the bound is an enumerator left as an identifier (not injected into ScannerInfo — that
    // OOM'd Melee-scale corpora), fall back to a constant table built from merged enums.
    // See https://github.com/eclipse-cdt/cdt/blob/main/core/org.eclipse.cdt.core/parser/org/eclipse/cdt/internal/core/dom/parser/ValueFactory.java
    declaratorChain(declarator).collectFirst(Function.unlift {
      case arrayDeclarator: IASTArrayDeclarator =>
        arrayDeclarator.getArrayModifiers.toList.collectFirst(Function.unlift { modifier =>
          Option(modifier.getConstantExpression).flatMap { expr =>
            Option(ValueFactory.getConstantNumericalValue(expr))
              .map(_.intValue())
              .orElse(constantLookup.get(expr.getRawSignature.trim))
          }
        })
      case _ =>
        None
    })
  }

  def initializerElementCount(declarator: IASTDeclarator): Option[Int] =
    Option(declarator.getInitializer).flatMap(initializerElementCount)

  private def initializerElementCount(initializer: IASTInitializer): Option[Int] =
    initializer match {
      case equalsInitializer: IASTEqualsInitializer =>
        Option(equalsInitializer.getInitializerClause).flatMap(countInitializerClause)
      case _ =>
        None
    }

  private def countInitializerClause(clause: IASTInitializerClause): Option[Int] =
    clause match {
      case initializerList: IASTInitializerList =>
        Some(initializerList.getClauses.length)
      case literal: IASTLiteralExpression
          if literal.getKind == IASTLiteralExpression.lk_string_literal =>
        Option(literal.getValue).map(value => stringLiteralByteLength(value.mkString))
      case _ =>
        Some(1)
    }

  private def stringLiteralByteLength(raw: String): Int = {
    val unquoted =
      if (raw.length >= 2 && raw.head == '"' && raw.last == '"') raw.substring(1, raw.length - 1)
      else raw
    unquoted.length + 1
  }

  private def extractGlobalDeclaratorWithInitializer(
      declaration: IASTDeclaration,
      typeName: String
  ): Option[IASTDeclarator] =
    declaration match {
      case simple: IASTSimpleDeclaration =>
        val declaredType = simple.getDeclSpecifier match {
          case composite: IASTCompositeTypeSpecifier if composite.getMembers.isEmpty =>
            normalizeTypeName(Option(composite.getName).map(_.toString).getOrElse(""))
          case _ =>
            normalizeTypeName(simple.getDeclSpecifier.getRawSignature)
        }
        if (declaredType != typeName) {
          None
        } else {
          simple.getDeclarators.toList.find(declarator =>
            Option(declarator.getInitializer).nonEmpty
          )
        }
      case _ =>
        None
    }

  private def extractFieldInitializerLengths(
      struct: IASTCompositeTypeSpecifier,
      structName: String,
      declarator: IASTDeclarator
  ): Map[(String, String), Int] = {
    val fields = extractFields(struct)
    val initializerClause =
      Option(declarator.getInitializer).flatMap(initializerElementCount).flatMap { _ =>
        Option(declarator.getInitializer).collect { case equals: IASTEqualsInitializer =>
          equals.getInitializerClause
        }
      }

    initializerClause match {
      case Some(clause: IASTInitializerList) =>
        // DESNOTE(jbarber, 2026-07-19): Positional zip against flattened extractFields misaligns
        // when anonymous/named unions expand to multiple members for one initializer slot. Collapse
        // each unionGroup to a single slot; if slot count still disagrees with the clause list,
        // refuse rather than attach unsized-array lengths to the wrong fields.
        val slots = initializerSlots(fields)
        val clauses = clause.getClauses.toList
        if (slots.length != clauses.length) {
          Map.empty
        } else {
          slots
            .zip(clauses)
            .flatMap { case (field, fieldClause) =>
              if (isArrayDeclarator(field.declarator) && arrayLength(field.declarator).isEmpty) {
                countInitializerClause(fieldClause).map { count =>
                  (structName, fieldName(field.declarator)) -> count
                }
              } else {
                None
              }
            }
            .toMap
        }
      case _ =>
        Map.empty
    }
  }

  /** One C initializer slot per field, collapsing union members that share a
    * [[StructFieldDecl.unionGroup]].
    */
  private[daphttp] def initializerSlots(fields: List[StructFieldDecl]): List[StructFieldDecl] = {
    val seenUnionGroups = scala.collection.mutable.LinkedHashSet.empty[String]
    fields.filter { field =>
      field.unionGroup match {
        case Some(group) => seenUnionGroups.add(group)
        case None        => true
      }
    }
  }

  private def declaratorChain(declarator: IASTDeclarator): List[IASTDeclarator] = {
    Iterator
      .iterate(Option(declarator))(_.flatMap(d => Option(d.getNestedDeclarator)))
      .takeWhile(_.nonEmpty)
      .flatten
      .toList
  }

  def fieldName(declarator: IASTDeclarator): String = {
    extractDeclaratorName(declarator)
  }

  private def extractDeclaratorName(declarator: IASTDeclarator): String = {
    declaratorChain(declarator).lastOption
      .flatMap(d => Option(d.getName))
      .map(_.toString.trim)
      .getOrElse("")
  }

  def normalizeTypeName(raw: String): String = {
    val storageClasses =
      Set("static", "extern", "volatile", "register", "inline", "auto", "thread_local", "typedef")
    var normalized = raw.trim.replaceAll("\\s+", " ")
    var changed = true
    while (changed) {
      val previous = normalized
      normalized = normalized
        .stripPrefix("const ")
        .stripPrefix("struct ")
        .stripPrefix("union ")
        .stripPrefix("enum ")
        .stripSuffix(" const")
        .stripSuffix(" volatile")
      storageClasses.foreach { qualifier =>
        val prefixed = s"$qualifier "
        if (normalized.startsWith(prefixed)) {
          normalized = normalized.stripPrefix(prefixed)
        }
      }
      changed = normalized != previous
    }
    normalized
  }
}
