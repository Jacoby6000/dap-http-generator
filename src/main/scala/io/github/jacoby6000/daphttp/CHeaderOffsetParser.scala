package io.github.jacoby6000.daphttp

import scala.collection.mutable
import scala.util.matching.Regex

object CHeaderOffsetParser {
  private val StructStart: Regex = """(?:typedef\s+)?struct\s+(\w+)\s*\{""".r
  private val OffsetComment: Regex = """/\*\s*(0x[0-9A-Fa-f]+)\s*\*/""".r

  def parse(source: String): Map[(String, String), Int] = {
    val offsets = mutable.Map.empty[(String, String), Int]
    var currentStruct: Option[String] = None
    var depth = 0
    var activeOffset: Option[Int] = None

    source.linesIterator.foreach { rawLine =>
      val line = rawLine.trim
      if (line.isEmpty || line.startsWith("//")) {
        ()
      } else if (currentStruct.isEmpty) {
        StructStart.findFirstMatchIn(line).foreach { matched =>
          currentStruct = Some(matched.group(1))
          depth = line.count(_ == '{') - line.count(_ == '}')
        }
      } else {
        val lineOffset =
          OffsetComment
            .findFirstMatchIn(line)
            .flatMap(matchResult => parseHexInt(matchResult.group(1)))

        if (line.contains("union") && lineOffset.isDefined) {
          activeOffset = lineOffset
        } else if (line == "};" || line.endsWith("};")) {
          activeOffset = None
        }

        lineOffset.orElse(activeOffset).foreach { offset =>
          fieldNames(line).foreach { fieldName =>
            currentStruct.foreach(structName => offsets.update((structName, fieldName), offset))
          }
        }

        depth += line.count(_ == '{') - line.count(_ == '}')
        if (depth <= 0) {
          currentStruct = None
          activeOffset = None
          depth = 0
        }
      }
    }

    offsets.toMap
  }

  private def fieldNames(line: String): List[String] = {
    if (!line.contains(";")) {
      Nil
    } else {
      val beforeSemi = line.takeWhile(_ != ';').trim
      val lastToken = beforeSemi.split("\\s+").lastOption.getOrElse("")
      val cleaned = lastToken.stripPrefix("*").filter(ch => ch.isLetterOrDigit || ch == '_')
      Option.when(cleaned.nonEmpty && !isReservedWord(cleaned))(cleaned).toList
    }
  }

  private def isReservedWord(name: String): Boolean =
    Set(
      "struct",
      "union",
      "enum",
      "typedef",
      "const",
      "static",
      "extern",
      "volatile",
      "u8",
      "u16",
      "u32",
      "u64",
      "s8",
      "s16",
      "s32",
      "s64",
      "int",
      "char",
      "void",
      "float",
      "double",
      "bool"
    ).contains(name)

  private def parseHexInt(raw: String): Option[Int] = {
    val trimmed = raw.trim
    if (trimmed.startsWith("0x") || trimmed.startsWith("0X")) {
      scala.util.Try(Integer.parseInt(trimmed.drop(2), 16)).toOption
    } else {
      scala.util.Try(Integer.parseInt(trimmed, 16)).toOption
    }
  }
}
