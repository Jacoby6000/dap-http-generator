package io.github.jacoby6000.daphttp

import org.eclipse.cdt.core.dom.ast.IASTArrayDeclarator
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression
import org.eclipse.cdt.core.dom.ast.IASTCompositeTypeSpecifier
import org.eclipse.cdt.core.dom.ast.IASTDeclSpecifier
import org.eclipse.cdt.core.dom.ast.IASTDeclaration
import org.eclipse.cdt.core.dom.ast.IASTDeclarator
import org.eclipse.cdt.core.dom.ast.IASTEnumerationSpecifier
import org.eclipse.cdt.core.dom.ast.IASTEqualsInitializer
import org.eclipse.cdt.core.dom.ast.IASTExpression
import org.eclipse.cdt.core.dom.ast.IASTFieldDeclarator
import org.eclipse.cdt.core.dom.ast.IASTFunctionDeclarator
import org.eclipse.cdt.core.dom.ast.IASTIdExpression
import org.eclipse.cdt.core.dom.ast.IASTInitializer
import org.eclipse.cdt.core.dom.ast.IASTInitializerClause
import org.eclipse.cdt.core.dom.ast.IASTInitializerList
import org.eclipse.cdt.core.dom.ast.IASTLiteralExpression
import org.eclipse.cdt.core.dom.ast.IASTSimpleDeclaration
import org.eclipse.cdt.core.dom.ast.IASTStandardFunctionDeclarator
import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit
import org.eclipse.cdt.core.dom.ast.IASTUnaryExpression
import org.eclipse.cdt.core.dom.ast.gnu.c.GCCLanguage
import org.eclipse.cdt.core.model.ILanguage
import org.eclipse.cdt.core.parser.DefaultLogService
import org.eclipse.cdt.core.parser.FileContent
import org.eclipse.cdt.core.parser.IncludeFileContentProvider
import org.eclipse.cdt.core.parser.ScannerInfo

import scala.jdk.CollectionConverters._
import scala.util.Try
import scala.util.control.NonFatal

final case class GlobalVariableDeclaration(
    name: String,
    typeName: String,
    isArray: Boolean,
    declaratorLength: Option[Int],
    initializerLength: Option[Int],
    pointerDepth: Int
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
  ): Map[String, String] = {
    val fromAst = parseTranslationUnit(source, "header.h", extraMacros)
      .map(_.getDeclarations.toList.flatMap(extractTypedef).toMap)
      .getOrElse(Map.empty)
    fromAst ++ extractDefineMacros(source)
  }

  def parseEnums(
      source: String,
      extraMacros: Map[String, String] = Map.empty
  ): Map[String, CEnumDefinition] = {
    val macros = BuiltInMacros ++ extraMacros ++ extractDefineMacros(source)
    parseTranslationUnit(source, "header.h", extraMacros)
      .map(_.getDeclarations.toList.flatMap(extractEnumDefinitions(_, macros)).toMap)
      .getOrElse(Map.empty)
  }

  private[daphttp] def extractDefineMacros(source: String): Map[String, String] = {
    val definePattern = """^\s*#\s*define\s+(\w+)\s+(\S.*)$""".r
    source.linesIterator.flatMap { line =>
      line match {
        case definePattern(name, value) =>
          val v = value.trim
          if (v.nonEmpty && !v.startsWith("(")) Some(name -> v)
          else None
        case _ => None
      }
    }.toMap
  }

  def parseGlobalDeclarations(
      source: String,
      extraMacros: Map[String, String] = Map.empty
  ): List[GlobalVariableDeclaration] =
    parseTranslationUnit(source, "source.c", extraMacros)
      .map(_.getDeclarations.toList.flatMap(extractGlobalDeclaration(_, extraMacros)))
      .getOrElse(Nil)

  def parseStructFieldInitializerLengths(
      source: String,
      structs: Map[String, IASTCompositeTypeSpecifier],
      extraMacros: Map[String, String] = Map.empty
  ): Map[(String, String), Int] =
    parseTranslationUnit(source, "source.c", extraMacros)
      .map { translationUnit =>
        translationUnit.getDeclarations.toList.flatMap { declaration =>
          extractGlobalDeclaration(declaration, extraMacros).flatMap { global =>
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
      declaration: IASTDeclaration,
      macros: Map[String, String] = Map.empty
  ): List[GlobalVariableDeclaration] = {
    declaration match {
      case simple: IASTSimpleDeclaration =>
        simple.getDeclSpecifier match {
          case composite: IASTCompositeTypeSpecifier if composite.getMembers.nonEmpty =>
            Nil
          case composite: IASTCompositeTypeSpecifier =>
            val typeName =
              normalizeTypeName(Option(composite.getName).map(_.toString).getOrElse(""))
            if (typeName.isEmpty) Nil else extractGlobalDeclarators(typeName, simple, macros)
          case _ =>
            val typeName = normalizeTypeName(simple.getDeclSpecifier.getRawSignature)
            if (typeName.isEmpty) Nil else extractGlobalDeclarators(typeName, simple, macros)
        }
      case _ =>
        Nil
    }
  }

  private[daphttp] def extractGlobalDeclarators(
      typeName: String,
      simple: IASTSimpleDeclaration,
      macros: Map[String, String] = Map.empty
  ): List[GlobalVariableDeclaration] =
    simple.getDeclarators.toList.flatMap { declarator =>
      val name = extractDeclaratorName(declarator)
      if (name.isEmpty || isFunctionDeclarator(declarator)) {
        Nil
      } else {
        val isArray = isArrayDeclarator(declarator)
        val declaratorLength = if (isArray) arrayLength(declarator, macros) else None
        val initializerLength = if (isArray) initializerElementCount(declarator) else None
        List(
          GlobalVariableDeclaration(
            name = name,
            typeName = typeName,
            isArray = isArray,
            declaratorLength = declaratorLength,
            initializerLength = initializerLength,
            pointerDepth = pointerDepth(declarator)
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
      macros: Map[String, String]
  ): List[(String, CEnumDefinition)] =
    declaration match {
      case simple: IASTSimpleDeclaration =>
        simple.getDeclSpecifier match {
          case enumSpec: IASTEnumerationSpecifier =>
            val tagName =
              Option(enumSpec.getName).map(_.toString.trim).filter(_.nonEmpty)
            val aliases = simple.getDeclarators.toList
              .map(extractDeclaratorName)
              .filter(_.nonEmpty)
            val names = (tagName.toList ++ aliases).distinct
            if (names.isEmpty) {
              Nil
            } else {
              val values = extractEnumeratorValues(enumSpec, macros)
              val primaryName = names.head
              val definition = CEnumDefinition(primaryName, values)
              names.map(_ -> definition)
            }
          case _ =>
            Nil
        }
      case _ =>
        Nil
    }

  private def extractEnumeratorValues(
      enumSpec: IASTEnumerationSpecifier,
      macros: Map[String, String]
  ): List[IrEnumValue] = {
    val known = scala.collection.mutable.LinkedHashMap.empty[String, Int]
    var nextValue = 0
    enumSpec.getEnumerators.toList.foreach { enumerator =>
      val name =
        Option(enumerator.getName).map(_.toString.trim).filter(_.nonEmpty).getOrElse("")
      if (name.nonEmpty) {
        val value = Option(enumerator.getValue)
          .flatMap(expr => evaluateIntExpression(expr, known.toMap, macros))
          .getOrElse(nextValue)
        known(name) = value
        nextValue = value + 1
      }
    }
    known.toList.map { case (name, value) => IrEnumValue(name, value) }
  }

  private def evaluateIntExpression(
      expression: IASTExpression,
      known: Map[String, Int],
      macros: Map[String, String]
  ): Option[Int] =
    expression match {
      case literal: IASTLiteralExpression
          if literal.getKind == IASTLiteralExpression.lk_integer_constant =>
        parseIntegerLiteral(literal.toString)
      case unary: IASTUnaryExpression =>
        val operand = evaluateIntExpression(unary.getOperand, known, macros)
        unary.getOperator match {
          case IASTUnaryExpression.op_plus             => operand
          case IASTUnaryExpression.op_minus            => operand.map(v => -v)
          case IASTUnaryExpression.op_tilde            => operand.map(v => ~v)
          case IASTUnaryExpression.op_bracketedPrimary => operand
          case _                                       => None
        }
      case binary: IASTBinaryExpression =>
        for {
          left <- evaluateIntExpression(binary.getOperand1, known, macros)
          right <- evaluateIntExpression(binary.getOperand2, known, macros)
          result <- binary.getOperator match {
            case IASTBinaryExpression.op_plus                 => Some(left + right)
            case IASTBinaryExpression.op_minus                => Some(left - right)
            case IASTBinaryExpression.op_multiply             => Some(left * right)
            case IASTBinaryExpression.op_divide if right != 0 =>
              Some(left / right)
            case IASTBinaryExpression.op_modulo if right != 0 =>
              Some(left % right)
            case IASTBinaryExpression.op_shiftLeft  => Some(left << right)
            case IASTBinaryExpression.op_shiftRight => Some(left >> right)
            case IASTBinaryExpression.op_binaryAnd  => Some(left & right)
            case IASTBinaryExpression.op_binaryOr   => Some(left | right)
            case IASTBinaryExpression.op_binaryXor  => Some(left ^ right)
            case _                                  => None
          }
        } yield result
      case id: IASTIdExpression =>
        val name = Option(id.getName).map(_.toString.trim).filter(_.nonEmpty)
        name.flatMap { n =>
          known
            .get(n)
            .orElse(macros.get(n).flatMap(parseIntegerLiteral))
        }
      case _ =>
        None
    }

  private def parseIntegerLiteral(raw: String): Option[Int] = {
    val cleaned = raw.trim.toLowerCase
      .stripSuffix("ull")
      .stripSuffix("llu")
      .stripSuffix("ul")
      .stripSuffix("lu")
      .stripSuffix("ll")
      .stripSuffix("u")
      .stripSuffix("l")
    if (cleaned.startsWith("0x")) {
      Try(Integer.parseUnsignedInt(cleaned.drop(2), 16)).toOption
        .orElse(Try(java.lang.Long.parseUnsignedLong(cleaned.drop(2), 16).toInt).toOption)
    } else if (cleaned.startsWith("0b")) {
      Try(Integer.parseInt(cleaned.drop(2), 2)).toOption
    } else if (cleaned.startsWith("0") && cleaned.length > 1 && cleaned.forall(_.isDigit)) {
      Try(Integer.parseInt(cleaned, 8)).toOption
    } else {
      Try(cleaned.toInt).toOption
    }
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
    declaratorChain(declarator).collectFirst { case field: IASTFieldDeclarator =>
      Option(field.getBitFieldSize)
        .map(_.getRawSignature.trim)
        .flatMap(_.toIntOption)
    }.flatten

  def pointerDepth(declarator: IASTDeclarator): Int = {
    declaratorChain(declarator).map(_.getPointerOperators.length).sum
  }

  def arrayLength(
      declarator: IASTDeclarator,
      macros: Map[String, String] = Map.empty
  ): Option[Int] = {
    declaratorChain(declarator).collectFirst(Function.unlift {
      case arrayDeclarator: IASTArrayDeclarator =>
        arrayDeclarator.getArrayModifiers.toList.collectFirst(Function.unlift { modifier =>
          Option(modifier.getConstantExpression)
            .map(_.getRawSignature.trim)
            .flatMap(parseArraySize)
            .orElse {
              Option(modifier.getConstantExpression)
                .map(_.getRawSignature.trim)
                .flatMap(s => macros.get(s).flatMap(parseArraySize))
            }
        })
      case _ =>
        None
    })
  }

  private def parseArraySize(s: String): Option[Int] = {
    val trimmed = s.trim
    if (trimmed.startsWith("0x") || trimmed.startsWith("0X")) {
      scala.util.Try(java.lang.Integer.parseUnsignedInt(trimmed.drop(2), 16)).toOption
    } else {
      trimmed.toIntOption
    }
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
        fields
          .zip(clause.getClauses)
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
      case _ =>
        Map.empty
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
