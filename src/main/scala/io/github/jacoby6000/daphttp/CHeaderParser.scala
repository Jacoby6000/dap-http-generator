package io.github.jacoby6000.daphttp

import atto._

import scala.util.matching.Regex

import Atto._

object CHeaderParser {
  private val StructKeyword = "typedef struct"

  private val FieldPattern: Regex =
    "^([A-Za-z_][A-Za-z0-9_]*)\\s*(\\*)?\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*(?:\\[\\s*(\\d+)\\s*\\])?$".r

  private val Identifier: Parser[String] =
    many1(letterOrDigit | char('_')).map(_.toList.mkString)

  private val Whitespace: Parser[Unit] = skipMany(oneOf(" \t\r\n"))

  private def token[A](parser: Parser[A]): Parser[A] = Whitespace ~> parser <~ Whitespace

  private val Struct: Parser[CStruct] =
    for {
      _ <- token(string("typedef"))
      _ <- token(string("struct"))
      _ <- opt(token(Identifier))
      _ <- token(char('{'))
      body <- takeWhile(_ != '}')
      _ <- token(char('}'))
      structName <- token(Identifier)
      _ <- token(char(';'))
    } yield CStruct(structName, parseFields(body))

  def parse(headerSource: String): List[CStruct] = parseStructs(stripComments(headerSource))

  private def parseStructs(content: String): List[CStruct] = {
    val structs = scala.collection.mutable.ListBuffer.empty[CStruct]
    var offset = 0

    while (offset < content.length) {
      val structStart = content.indexOf(StructKeyword, offset)
      if (structStart < 0) {
        offset = content.length
      } else {
        extractStructSnippet(content, structStart) match {
          case Some((snippet, nextOffset)) =>
            Struct.parseOnly(snippet).either match {
              case Right(parsedStruct) => structs += parsedStruct
              case Left(_)             => ()
            }
            offset = nextOffset
          case None =>
            offset = structStart + StructKeyword.length
        }
      }
    }

    structs.toList
  }

  private def extractStructSnippet(content: String, start: Int): Option[(String, Int)] = {
    val openBrace = content.indexOf('{', start)
    if (openBrace < 0) {
      None
    } else {
      val closeBrace = content.indexOf('}', openBrace + 1)
      if (closeBrace < 0) {
        None
      } else {
        val semicolon = content.indexOf(';', closeBrace + 1)
        if (semicolon < 0) {
          None
        } else {
          Some((content.substring(start, semicolon + 1), semicolon + 1))
        }
      }
    }
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
