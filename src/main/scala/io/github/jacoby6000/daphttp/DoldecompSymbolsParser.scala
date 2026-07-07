package io.github.jacoby6000.daphttp

import scala.util.Try
import scala.util.matching.Regex

final case class DoldecompSymbol(
    name: String,
    section: String,
    address: Long,
    attributes: Map[String, String]
) {
  def symbolType: Option[String] = attributes.get("type")
  def cType: Option[String] = attributes.get("ctype")
  def sizeBytes: Option[Int] = attributes.get("size").flatMap(DoldecompSymbolsParser.parseInt)
}

object DoldecompSymbolsParser {
  private val SymbolPattern: Regex =
    "^\\s*([^=\\s]+)\\s*=\\s*([^:;]+):([^;]+);\\s*(?://\\s*(.*))?$".r

  def parse(content: String): Either[List[String], List[DoldecompSymbol]] = {
    val errors = scala.collection.mutable.ListBuffer.empty[String]
    val symbols = content.linesIterator.zipWithIndex.flatMap { case (line, idx) =>
      val trimmed = line.trim
      if (trimmed.isEmpty || trimmed.startsWith("//") || trimmed.startsWith("#")) {
        None
      } else {
        trimmed match {
          case SymbolPattern(name, section, addressRaw, attributesRaw) =>
            parseAddress(addressRaw.trim) match {
              case Some(address) =>
                Some(
                  DoldecompSymbol(
                    name = name.trim,
                    section = section.trim,
                    address = address,
                    attributes = parseAttributes(Option(attributesRaw).getOrElse("").trim)
                  )
                )
              case None =>
                errors += s"symbols.txt:${idx + 1}: Invalid address '$addressRaw'."
                None
            }
          case _ =>
            errors += s"symbols.txt:${idx + 1}: Invalid symbol line format."
            None
        }
      }
    }.toList

    if (errors.nonEmpty) Left(errors.toList) else Right(symbols)
  }

  private[daphttp] def parseInt(value: String): Option[Int] = {
    val trimmed = value.trim
    if (trimmed.startsWith("0x") || trimmed.startsWith("0X")) {
      Try(java.lang.Integer.parseUnsignedInt(trimmed.drop(2), 16)).toOption
    } else {
      Try(trimmed.toInt).toOption
    }
  }

  private def parseAddress(value: String): Option[Long] = {
    val trimmed = value.trim
    if (trimmed.startsWith("0x") || trimmed.startsWith("0X")) {
      Try(java.lang.Long.parseUnsignedLong(trimmed.drop(2), 16)).toOption
    } else {
      Try(trimmed.toLong).toOption
    }
  }

  private def parseAttributes(raw: String): Map[String, String] = {
    raw
      .split("\\s+")
      .toList
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap { token =>
        token.split(":", 2).toList match {
          case key :: value :: Nil => Some(key -> value)
          case key :: Nil          => Some(key -> "true")
          case _                   => None
        }
      }
      .toMap
  }
}
