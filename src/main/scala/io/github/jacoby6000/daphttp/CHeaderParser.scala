package io.github.jacoby6000.daphttp

import scala.util.matching.Regex

object CHeaderParser {
  private val StructPattern: Regex =
    "(?s)typedef\\s+struct\\s+(?:[A-Za-z_][A-Za-z0-9_]*\\s+)?\\{(.*?)\\}\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*;"
      .r

  private val FieldPattern: Regex =
    "^([A-Za-z_][A-Za-z0-9_]*)\\s*(\\*)?\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*(?:\\[\\s*(\\d+)\\s*\\])?$".r

  def parse(headerSource: String): CHeaderAst = {
    val cleaned = stripComments(headerSource)
    val structs = StructPattern.findAllMatchIn(cleaned).toList.map { raw =>
      val body = raw.group(1)
      val structName = raw.group(2)
      CStruct(structName, parseFields(body))
    }
    CHeaderAst(structs)
  }

  private def stripComments(content: String): String = {
    content
      .replaceAll("(?s)/\\*.*?\\*/", "")
      .replaceAll("(?m)//.*$", "")
  }

  private def parseFields(body: String): List[CField] = {
    body
      .split(";")
      .toList
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap {
        case FieldPattern(typeName, pointerRaw, fieldName, arrayLengthRaw) =>
          Some(
            CField(
              typeName = typeName.trim,
              name = fieldName.trim,
              isPointer = Option(pointerRaw).exists(_.nonEmpty),
              arrayLength = Option(arrayLengthRaw).flatMap(_.toIntOption)
            )
          )
        case _ => None
      }
  }
}
