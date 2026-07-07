package io.github.jacoby6000.daphttp

import java.nio.file.Files
import java.nio.file.Path
import scala.jdk.CollectionConverters._

object DoldecompSmithyGenerator {
  private final case class PrimitiveShape(target: String, traitName: Option[String])
  private final case class OperationModel(
      symbol: DoldecompSymbol,
      operationName: String,
      outputName: String,
      rootStructName: String
  )
  private final case class ListAlias(name: String, elementType: String)

  def generateFromPaths(
      symbolsPath: Path,
      headerRoots: List[Path],
      namespace: String = "doldecomp.generated",
      serviceName: String = "DolDecompApi",
      wordSizeBits: Int = 32
  ): Either[List[String], String] = {
    val symbolsContent = new String(Files.readAllBytes(symbolsPath))
    val symbols = DoldecompSymbolsParser.parse(symbolsContent)
    symbols.flatMap(generateFromSymbols(_, headerRoots, namespace, serviceName, wordSizeBits))
  }

  def generateFromSymbols(
      symbols: List[DoldecompSymbol],
      headerRoots: List[Path],
      namespace: String = "doldecomp.generated",
      serviceName: String = "DolDecompApi",
      wordSizeBits: Int = 32
  ): Either[List[String], String] = {
    val headerStructs = loadStructs(headerRoots)
    val objectSymbols = symbols.filter(_.symbolType.contains("object")).filter(_.cType.nonEmpty)
    if (objectSymbols.isEmpty) {
      Left(List("No object symbols with ctype metadata were found."))
    } else {
      val errors = scala.collection.mutable.ListBuffer.empty[String]
      val operations = objectSymbols.map { symbol =>
        val operationName = s"Get${toPascalCase(symbol.name)}"
        val outputName = s"${operationName}Output"
        val rootStructName = symbol.cType.get
        if (!headerStructs.contains(rootStructName)) {
          errors += s"${symbol.name}: Missing struct definition for ctype '$rootStructName'."
        }
        OperationModel(symbol, operationName, outputName, rootStructName)
      }

      if (errors.nonEmpty) {
        Left(errors.toList)
      } else {
        Right(renderSmithy(namespace, serviceName, wordSizeBits, operations, headerStructs))
      }
    }
  }

  private def loadStructs(headerRoots: List[Path]): Map[String, CStruct] = {
    val headerFiles = headerRoots.flatMap(collectHeaderFiles).distinct
    headerFiles.flatMap { path =>
      val source = new String(Files.readAllBytes(path))
      CHeaderParser.parse(source).structs
    }.map(struct => struct.name -> struct).toMap
  }

  private def collectHeaderFiles(root: Path): List[Path] = {
    if (!Files.exists(root)) {
      Nil
    } else if (Files.isRegularFile(root) && root.toString.endsWith(".h")) {
      List(root)
    } else if (Files.isDirectory(root)) {
      val stream = Files.walk(root)
      try {
        stream
          .iterator()
          .asScala
          .filter(path => Files.isRegularFile(path) && path.toString.endsWith(".h"))
          .toList
      } finally {
        stream.close()
      }
    } else {
      Nil
    }
  }

  private def renderSmithy(
      namespace: String,
      serviceName: String,
      wordSizeBits: Int,
      operations: List[OperationModel],
      headerStructs: Map[String, CStruct]
  ): String = {
    val usedTraits = scala.collection.mutable.LinkedHashSet("dapStruct", "staticAddress", "wordSize")
    val reachableStructs = collectReachableStructs(operations, headerStructs)
    val listAliases = scala.collection.mutable.LinkedHashSet.empty[ListAlias]

    val operationLines = operations.map(_.operationName).mkString(", ")
    val outputBlocks = operations.map { operation =>
      s"""operation ${operation.operationName} {
         |    output: ${operation.outputName}
         |}
         |
         |structure ${operation.outputName} {
         |    @staticAddress("${formatHex(operation.symbol.address)}")
         |    value: ${operation.rootStructName}
         |}""".stripMargin
    }

    val structBlocks = reachableStructs.map { struct =>
      val members = struct.fields.flatMap { field =>
        toMemberLines(struct.name, field, reachableStructs.map(_.name).toSet, listAliases, usedTraits)
      }
      s"""@dapStruct
         |structure ${struct.name} {
         |${members.mkString("\n")}
         |}""".stripMargin
    }

    val listBlocks = listAliases.toList.map(alias =>
      s"""list ${alias.name} {
         |    member: ${alias.elementType}
         |}""".stripMargin
    )

    val useLines = usedTraits.toList.sorted.map(name => s"use com.jacoby6000.daphttp#$name")

    s"""$${version: "2"}
       |
       |namespace $namespace
       |
       |${useLines.mkString("\n")}
       |
       |@wordSize($wordSizeBits)
       |service $serviceName {
       |    version: "1"
       |    operations: [$operationLines]
       |}
       |
       |${outputBlocks.mkString("\n\n")}
       |
       |${structBlocks.mkString("\n\n")}
       |
       |${listBlocks.mkString("\n\n")}""".stripMargin
  }

  private def collectReachableStructs(
      operations: List[OperationModel],
      headerStructs: Map[String, CStruct]
  ): List[CStruct] = {
    val visited = scala.collection.mutable.LinkedHashSet.empty[String]

    def visit(structName: String): Unit = {
      if (!visited.contains(structName)) {
        visited += structName
        headerStructs.get(structName).foreach { struct =>
          struct.fields.foreach { field =>
            val normalized = normalizeTypeName(field.typeName)
            if (headerStructs.contains(normalized)) {
              visit(normalized)
            }
          }
        }
      }
    }

    operations.foreach(operation => visit(operation.rootStructName))
    visited.toList.flatMap(headerStructs.get)
  }

  private def toMemberLines(
      structName: String,
      field: CField,
      knownStructs: Set[String],
      listAliases: scala.collection.mutable.LinkedHashSet[ListAlias],
      usedTraits: scala.collection.mutable.LinkedHashSet[String]
  ): List[String] = {
    val normalizedType = normalizeTypeName(field.typeName)
    val memberName = toCamelCase(field.name)
    if (knownStructs.contains(normalizedType)) {
      List(s"    $memberName: $normalizedType")
    } else {
      primitiveForType(normalizedType) match {
        case Some(primitive) =>
          val traitAnnotations = scala.collection.mutable.ListBuffer.empty[String]
          primitive.traitName.foreach { traitName =>
            usedTraits += traitName
            traitAnnotations += s"    @$traitName"
          }
          if (field.isPointer) {
            usedTraits += "pointer"
            traitAnnotations += "    @pointer"
          }

          field.arrayLength match {
            case Some(length) =>
              val aliasName = s"${structName}${toPascalCase(field.name)}Array"
              listAliases += ListAlias(aliasName, primitive.target)
              usedTraits += "array"
              usedTraits += "length"
              traitAnnotations ++= List("    @array", s"    @length($length)")
              traitAnnotations.toList :+ s"    $memberName: $aliasName"
            case None =>
              traitAnnotations.toList :+ s"    $memberName: ${primitive.target}"
          }
        case None =>
          List(s"    $memberName: Long")
      }
    }
  }

  private def primitiveForType(typeName: String): Option[PrimitiveShape] = {
    val normalized = normalizeTypeName(typeName)
    normalized match {
      case "u8"    => Some(PrimitiveShape("Integer", Some("u8")))
      case "s8"    => Some(PrimitiveShape("Integer", Some("s8")))
      case "u16"   => Some(PrimitiveShape("Integer", Some("u16")))
      case "s16"   => Some(PrimitiveShape("Integer", Some("s16")))
      case "u32"   => Some(PrimitiveShape("Integer", Some("u32")))
      case "s32"   => Some(PrimitiveShape("Integer", Some("s32")))
      case "u64"   => Some(PrimitiveShape("Long", Some("u64")))
      case "s64"   => Some(PrimitiveShape("Long", Some("s64")))
      case "u128"  => Some(PrimitiveShape("Long", Some("u128")))
      case "s128"  => Some(PrimitiveShape("Long", Some("s128")))
      case "f32"   => Some(PrimitiveShape("Float", Some("f32")))
      case "f64"   => Some(PrimitiveShape("Double", Some("f64")))
      case "char"  => Some(PrimitiveShape("Byte", Some("char")))
      case "bool"  => Some(PrimitiveShape("Boolean", None))
      case "byte"  => Some(PrimitiveShape("Byte", None))
      case "short" => Some(PrimitiveShape("Short", None))
      case "int"   => Some(PrimitiveShape("Integer", None))
      case "long"  => Some(PrimitiveShape("Long", None))
      case _       => None
    }
  }

  private def normalizeTypeName(value: String): String = {
    value
      .trim
      .stripPrefix("const ")
      .stripPrefix("struct ")
      .replaceAll("\\s+", "")
  }

  private def toPascalCase(value: String): String =
    value
      .split("[^A-Za-z0-9]+")
      .toList
      .filter(_.nonEmpty)
      .map(part => part.head.toUpper + part.drop(1))
      .mkString

  private def toCamelCase(value: String): String = {
    val pascal = toPascalCase(value)
    if (pascal.isEmpty) "value" else pascal.head.toLower + pascal.drop(1)
  }

  private def formatHex(value: Long): String = f"0x$value%x"
}
