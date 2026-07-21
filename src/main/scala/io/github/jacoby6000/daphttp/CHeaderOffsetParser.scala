package io.github.jacoby6000.daphttp

import scala.collection.mutable
import scala.util.matching.Regex

object CHeaderOffsetParser {
  private val StructStart: Regex = """(?:typedef\s+)?struct\s+(\w+)\s*\{""".r
  // DESNOTE(jbarber, 2026-07-20): Melee/doldecomp headers use both `/* 0x04 */` and `/* +0x04 */`
  // / `/* +194 */` (hex without 0x). Treat the optional `+` form as hex offsets.
  private val OffsetComment: Regex = """/\*\s*\+?(0x[0-9A-Fa-f]+|[0-9A-Fa-f]+)\s*\*/""".r
  private val BlockComment: Regex = """/\*.*?\*/""".r
  private val FunctionPointerName: Regex =
    """\(\s*\*+\s*([A-Za-z_]\w*)\s*(?:\[[^\]]*\]\s*)*\)""".r
  private val Identifier: Regex = """[A-Za-z_]\w*""".r

  def parse(source: String): Map[(String, String), Int] = {
    val offsets = mutable.Map.empty[(String, String), Int]
    // (structName, minimum depth while the struct body is open)
    var frames = List.empty[(String, Int)]
    var depth = 0
    var activeOffset: Option[Int] = None

    source.linesIterator.foreach { rawLine =>
      val line = rawLine.trim
      if (line.isEmpty || line.startsWith("//")) {
        ()
      } else {
        val opens = line.count(_ == '{')
        val closes = line.count(_ == '}')

        StructStart.findFirstMatchIn(line).foreach { matched =>
          // Regex requires `{` on the same line; body is active at depth after that open.
          frames = (matched.group(1), depth + opens) :: frames
        }

        if (frames.nonEmpty) {
          val currentStruct = frames.head._1
          val lineOffset =
            OffsetComment
              .findFirstMatchIn(line)
              .flatMap(matchResult => parseHexInt(matchResult.group(1)))

          if (line.contains("union") && lineOffset.isDefined) {
            activeOffset = lineOffset
          }

          val names = fieldNames(line)
          lineOffset match {
            case Some(offset) if activeOffset.isDefined && !line.contains("union") =>
              names.foreach(fieldName => offsets.update((currentStruct, fieldName), offset))
            case Some(offset) =>
              names.headOption.foreach(fieldName =>
                offsets.update((currentStruct, fieldName), offset)
              )
            case None =>
              activeOffset.foreach { offset =>
                names.foreach(fieldName => offsets.update((currentStruct, fieldName), offset))
              }
          }

          if (closes > 0 && activeOffset.isDefined) {
            activeOffset = None
          }
        }

        depth += opens - closes
        while (frames.nonEmpty && depth < frames.head._2) {
          frames = frames.tail
          activeOffset = None
        }
        if (frames.isEmpty) {
          depth = 0
          activeOffset = None
        }
      }
    }

    offsets.toMap
  }

  private def fieldNames(line: String): List[String] = {
    if (!line.contains(";")) {
      Nil
    } else {
      val beforeSemi = BlockComment.replaceAllIn(line.takeWhile(_ != ';'), " ").trim
      splitDeclarators(beforeSemi).flatMap(declaratorName)
    }
  }

  private def splitDeclarators(declaration: String): List[String] = {
    val parts = mutable.ListBuffer.empty[String]
    val current = new StringBuilder
    var parentheses = 0
    var brackets = 0
    var braces = 0

    declaration.foreach {
      case ',' if parentheses == 0 && brackets == 0 && braces == 0 =>
        parts += current.result()
        current.clear()
      case ch =>
        current.append(ch)
        ch match {
          case '(' => parentheses += 1
          case ')' => parentheses = math.max(0, parentheses - 1)
          case '[' => brackets += 1
          case ']' => brackets = math.max(0, brackets - 1)
          case '{' => braces += 1
          case '}' => braces = math.max(0, braces - 1)
          case _   => ()
        }
    }
    parts += current.result()
    parts.toList
  }

  private def declaratorName(declarator: String): Option[String] =
    FunctionPointerName
      .findFirstMatchIn(declarator)
      .map(_.group(1))
      .orElse {
        val withoutArrays = declarator.replaceAll("""\[[^\]]*\]""", " ")
        val beforeBitfield = withoutArrays.takeWhile(_ != ':')
        Identifier
          .findAllIn(beforeBitfield)
          .toList
          .lastOption
      }
      .filterNot(isReservedWord)

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
