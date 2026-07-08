package io.github.jacoby6000.daphttp

import org.eclipse.cdt.core.dom.ast.IASTArrayDeclarator
import org.eclipse.cdt.core.dom.ast.IASTCompositeTypeSpecifier
import org.eclipse.cdt.core.dom.ast.IASTDeclaration
import org.eclipse.cdt.core.dom.ast.IASTDeclarator
import org.eclipse.cdt.core.dom.ast.IASTSimpleDeclaration
import org.eclipse.cdt.core.dom.ast.gnu.c.GCCLanguage
import org.eclipse.cdt.core.model.ILanguage
import org.eclipse.cdt.core.parser.DefaultLogService
import org.eclipse.cdt.core.parser.FileContent
import org.eclipse.cdt.core.parser.IncludeFileContentProvider
import org.eclipse.cdt.core.parser.ScannerInfo

import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

object CHeaderParser {
  def parse(headerSource: String): List[CStruct] = {
    val source = stripComments(headerSource)
    try {
      val translationUnit = GCCLanguage.getDefault.getASTTranslationUnit(
        FileContent.create("header.h", source.toCharArray),
        new ScannerInfo(Map.empty[String, String].asJava, Array.empty[String]),
        IncludeFileContentProvider.getEmptyFilesProvider,
        null,
        ILanguage.OPTION_SKIP_FUNCTION_BODIES,
        new DefaultLogService()
      )

      translationUnit.getDeclarations.toList.flatMap(extractStruct)
    } catch {
      case NonFatal(_) => Nil
    }
  }

  private def extractStruct(declaration: IASTDeclaration): Option[CStruct] = {
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
            structName.map(name => CStruct(name, extractFields(composite)))
          case _ =>
            None
        }
      case _ =>
        None
    }
  }

  private def extractFields(composite: IASTCompositeTypeSpecifier): List[CField] = {
    composite.getMembers.toList.flatMap {
      case member: IASTSimpleDeclaration =>
        val baseType = normalizeTypeName(member.getDeclSpecifier.getRawSignature)
        member.getDeclarators.toList.flatMap { declarator =>
          val fieldName = extractDeclaratorName(declarator)
          Option.when(fieldName.nonEmpty) {
            CField(
              typeName = baseType,
              name = fieldName,
              pointerDepth = pointerDepth(declarator),
              arrayLength = arrayLength(declarator)
            )
          }
        }
      case _ => Nil
    }
  }

  private def pointerDepth(declarator: IASTDeclarator): Int = {
    declaratorChain(declarator).map(_.getPointerOperators.length).sum
  }

  private def arrayLength(declarator: IASTDeclarator): Option[Int] = {
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

  private def declaratorChain(declarator: IASTDeclarator): List[IASTDeclarator] = {
    Iterator
      .iterate(Option(declarator))(_.flatMap(d => Option(d.getNestedDeclarator)))
      .takeWhile(_.nonEmpty)
      .flatten
      .toList
  }

  private def extractDeclaratorName(declarator: IASTDeclarator): String = {
    declaratorChain(declarator).lastOption
      .flatMap(d => Option(d.getName))
      .map(_.toString.trim)
      .getOrElse("")
  }

  private def normalizeTypeName(raw: String): String = {
    raw.trim
      .stripPrefix("const ")
      .stripPrefix("struct ")
      .replaceAll("\\s+", " ")
  }

  private def stripComments(content: String): String = {
    content
      .replaceAll("(?s)/\\*.*?\\*/", "")
      .replaceAll("(?m)//.*$", "")
  }
}
