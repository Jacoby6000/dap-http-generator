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
    warnings: List[String] = Nil
)

object CHeaderParser {
  def parse(
      headerSource: String,
      extraMacros: Map[String, String] = Map.empty
  ): List[(String, IASTCompositeTypeSpecifier)] =
    parseTranslationUnit(headerSource, "header.h", extraMacros)
      .map(_.getDeclarations.toList.flatMap(extractStructs))
      .getOrElse(Nil)

  def parseTypedefs(
      source: String,
      extraMacros: Map[String, String] = Map.empty
  ): Map[String, String] =
    parseTranslationUnit(source, "header.h", extraMacros)
      .map(_.getDeclarations.toList.flatMap(extractTypedef).toMap)
      .getOrElse(Map.empty)

  def parseEnums(
      source: String,
      extraMacros: Map[String, String] = Map.empty
  ): EnumParseResult = {
    // DESNOTE(jbarber, 2026-07-19): Pass macros through CDT ScannerInfo so the preprocessor
    // expands #define uses in enumerator initializers before we evaluate them. Prefer CDT's
    // ValueFactory over hand-rolled expression/macro evaluation.
    // See https://github.com/eclipse-cdt/cdt/blob/main/core/org.eclipse.cdt.core/parser/org/eclipse/cdt/internal/core/dom/parser/ValueFactory.java
    parseTranslationUnit(source, "header.h", extraMacros)
      .map { translationUnit =>
        val warnings = ListBuffer.empty[String]
        val enums = translationUnit.getDeclarations.toList
          .flatMap(extractEnumDefinitions(_, warnings))
          .toMap
        EnumParseResult(enums, warnings.toList)
      }
      .getOrElse(EnumParseResult(Map.empty, Nil))
  }

  private[daphttp] def extractMacros(source: String): Map[String, String] = {
    // DESNOTE(jbarber, 2026-07-19): Always collect macros from CDT's preprocessor AST — never
    // regex/#define line scraping. Expansions (including parenthesized bodies) feed ScannerInfo
    // for later translation units.
    parseTranslationUnit(source, "macros.h", Map.empty)
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
      extraMacros: Map[String, String] = Map.empty
  ): List[GlobalVariableDeclaration] =
    parseTranslationUnit(source, "source.c", extraMacros)
      .map(_.getDeclarations.toList.flatMap(extractGlobalDeclaration))
      .getOrElse(Nil)

  def parseStructFieldInitializerLengths(
      source: String,
      structs: Map[String, IASTCompositeTypeSpecifier],
      extraMacros: Map[String, String] = Map.empty
  ): Map[(String, String), Int] =
    parseTranslationUnit(source, "source.c", extraMacros)
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

  private def parseTranslationUnit(
      source: String,
      fileName: String,
      extraMacros: Map[String, String]
  ): Option[IASTTranslationUnit] = {
    val stripped = stripComments(source)
    val allMacros = BuiltInMacros ++ extraMacros
    try {
      Some(
        GCCLanguage.getDefault.getASTTranslationUnit(
          FileContent.create(fileName, stripped.toCharArray),
          new ScannerInfo(allMacros.asJava, Array.empty[String]),
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
      declaration: IASTDeclaration
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
              if (typeName.isEmpty) Nil else extractGlobalDeclarators(typeName, simple)
            }
          case _ =>
            val typeName = normalizeTypeName(simple.getDeclSpecifier.getRawSignature)
            if (typeName.isEmpty) Nil else extractGlobalDeclarators(typeName, simple)
        }
      case _ =>
        Nil
    }

  private[daphttp] def extractGlobalDeclarators(
      typeName: String,
      simple: IASTSimpleDeclaration
  ): List[GlobalVariableDeclaration] =
    simple.getDeclarators.toList.flatMap { declarator =>
      val name = extractDeclaratorName(declarator)
      if (name.isEmpty || isFunctionDeclarator(declarator)) {
        Nil
      } else {
        val isArray = isArrayDeclarator(declarator)
        val declaratorLength = if (isArray) arrayLength(declarator) else None
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

  def arrayLength(declarator: IASTDeclarator): Option[Int] = {
    // DESNOTE(jbarber, 2026-07-19): Array bounds come from CDT's already-preprocessed AST via
    // ValueFactory — do not re-parse raw signatures or look up macro names by hand.
    declaratorChain(declarator).collectFirst(Function.unlift {
      case arrayDeclarator: IASTArrayDeclarator =>
        arrayDeclarator.getArrayModifiers.toList.collectFirst(Function.unlift { modifier =>
          Option(modifier.getConstantExpression).flatMap { expr =>
            Option(ValueFactory.getConstantNumericalValue(expr)).map(_.intValue())
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

  private def stripComments(content: String): String = {
    content
      .replaceAll("(?s)/\\*.*?\\*/", "")
      .replaceAll("(?m)//.*$", "")
  }
}
