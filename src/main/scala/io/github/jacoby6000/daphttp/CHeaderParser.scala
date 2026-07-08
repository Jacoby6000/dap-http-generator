package io.github.jacoby6000.daphttp

import org.eclipse.cdt.core.dom.ast.IASTArrayDeclarator
import org.eclipse.cdt.core.dom.ast.IASTCompositeTypeSpecifier
import org.eclipse.cdt.core.dom.ast.IASTDeclaration
import org.eclipse.cdt.core.dom.ast.IASTDeclarator
import org.eclipse.cdt.core.dom.ast.IASTEqualsInitializer
import org.eclipse.cdt.core.dom.ast.IASTFunctionDeclarator
import org.eclipse.cdt.core.dom.ast.IASTInitializer
import org.eclipse.cdt.core.dom.ast.IASTInitializerClause
import org.eclipse.cdt.core.dom.ast.IASTInitializerList
import org.eclipse.cdt.core.dom.ast.IASTLiteralExpression
import org.eclipse.cdt.core.dom.ast.IASTSimpleDeclaration
import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit
import org.eclipse.cdt.core.dom.ast.gnu.c.GCCLanguage
import org.eclipse.cdt.core.model.ILanguage
import org.eclipse.cdt.core.parser.DefaultLogService
import org.eclipse.cdt.core.parser.FileContent
import org.eclipse.cdt.core.parser.IncludeFileContentProvider
import org.eclipse.cdt.core.parser.ScannerInfo

import scala.jdk.CollectionConverters._
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
    unionGroup: Option[String]
)

object CHeaderParser {
  def parse(headerSource: String): List[(String, IASTCompositeTypeSpecifier)] =
    parseTranslationUnit(headerSource, "header.h")
      .map(_.getDeclarations.toList.flatMap(extractStruct))
      .getOrElse(Nil)

  def parseGlobalDeclarations(source: String): List[GlobalVariableDeclaration] =
    parseTranslationUnit(source, "source.c")
      .map(_.getDeclarations.toList.flatMap(extractGlobalDeclaration))
      .getOrElse(Nil)

  def parseStructFieldInitializerLengths(
      source: String,
      structs: Map[String, IASTCompositeTypeSpecifier]
  ): Map[(String, String), Int] =
    parseTranslationUnit(source, "source.c")
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

  private def parseTranslationUnit(
      source: String,
      fileName: String
  ): Option[IASTTranslationUnit] = {
    val stripped = stripComments(source)
    try {
      Some(
        GCCLanguage.getDefault.getASTTranslationUnit(
          FileContent.create(fileName, stripped.toCharArray),
          new ScannerInfo(Map.empty[String, String].asJava, Array.empty[String]),
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

  private def extractGlobalDeclaration(
      declaration: IASTDeclaration
  ): List[GlobalVariableDeclaration] = {
    declaration match {
      case simple: IASTSimpleDeclaration =>
        simple.getDeclSpecifier match {
          case composite: IASTCompositeTypeSpecifier if composite.getMembers.nonEmpty =>
            Nil
          case composite: IASTCompositeTypeSpecifier =>
            val typeName =
              normalizeTypeName(Option(composite.getName).map(_.toString).getOrElse(""))
            if (typeName.isEmpty) Nil else extractGlobalDeclarators(typeName, simple)
          case _ =>
            val typeName = normalizeTypeName(simple.getDeclSpecifier.getRawSignature)
            if (typeName.isEmpty) Nil else extractGlobalDeclarators(typeName, simple)
        }
      case _ =>
        Nil
    }
  }

  private def extractGlobalDeclarators(
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
            pointerDepth = pointerDepth(declarator)
          )
        )
      }
    }

  private def isFunctionDeclarator(declarator: IASTDeclarator): Boolean =
    declaratorChain(declarator).exists(_.isInstanceOf[IASTFunctionDeclarator])

  private def isArrayDeclarator(declarator: IASTDeclarator): Boolean =
    declaratorChain(declarator).exists(_.isInstanceOf[IASTArrayDeclarator])

  def isArrayField(declarator: IASTDeclarator): Boolean = isArrayDeclarator(declarator)

  private def extractStruct(
      declaration: IASTDeclaration
  ): Option[(String, IASTCompositeTypeSpecifier)] = {
    declaration match {
      case simple: IASTSimpleDeclaration =>
        simple.getDeclSpecifier match {
          case composite: IASTCompositeTypeSpecifier
              if composite.getKey == IASTCompositeTypeSpecifier.k_struct =>
            val structName =
              simple.getDeclarators.toList
                .map(extractDeclaratorName)
                .find(_.nonEmpty)
                .orElse(Option(composite.getName).map(_.toString.trim).filter(_.nonEmpty))
            structName.map(name => name -> composite)
          case _ =>
            None
        }
      case _ =>
        None
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
                    StructFieldDecl(baseType, declarator, unionGroup)
                  }
                }
              case _ => Nil
            }
          case _ =>
            val baseType = normalizeTypeName(member.getDeclSpecifier.getRawSignature)
            member.getDeclarators.toList.flatMap { declarator =>
              Option.when(extractDeclaratorName(declarator).nonEmpty) {
                StructFieldDecl(baseType, declarator, unionGroup = None)
              }
            }
        }
      case _ => Nil
    }
  }

  def pointerDepth(declarator: IASTDeclarator): Int = {
    declaratorChain(declarator).map(_.getPointerOperators.length).sum
  }

  def arrayLength(declarator: IASTDeclarator): Option[Int] = {
    declaratorChain(declarator).collectFirst(Function.unlift {
      case arrayDeclarator: IASTArrayDeclarator =>
        arrayDeclarator.getArrayModifiers.toList.collectFirst(Function.unlift { modifier =>
          Option(modifier.getConstantExpression)
            .map(_.getRawSignature.trim)
            .flatMap(_.toIntOption)
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
      Set("static", "extern", "volatile", "register", "inline", "auto", "thread_local")
    var normalized = raw.trim.replaceAll("\\s+", " ")
    var changed = true
    while (changed) {
      val previous = normalized
      normalized = normalized.stripPrefix("const ").stripPrefix("struct ")
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
