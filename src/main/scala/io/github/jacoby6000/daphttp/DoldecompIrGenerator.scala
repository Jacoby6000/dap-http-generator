package io.github.jacoby6000.daphttp

import org.eclipse.cdt.core.dom.ast.IASTCompositeTypeSpecifier
import org.eclipse.cdt.core.dom.ast.IASTDeclarator
import software.amazon.smithy.model.shapes.ShapeId

import java.nio.file.Files
import java.nio.file.Path
import scala.collection.mutable
import scala.jdk.CollectionConverters._

object DoldecompIrGenerator {
  private final case class OperationModel(
      symbol: DoldecompSymbol,
      operationName: String,
      outputName: String,
      rootStructName: String
  )

  def generateFromPaths(
      symbolsPath: Path,
      headerRoots: List[Path],
      namespace: String = "doldecomp.generated",
      serviceName: String = "DolDecompApi",
      wordSizeBits: Int = 32
  ): Either[List[String], List[IrService]] = {
    val symbolsContent = new String(Files.readAllBytes(symbolsPath))
    DoldecompSymbolsParser
      .parse(symbolsContent)
      .flatMap(generateFromSymbols(_, headerRoots, namespace, serviceName, wordSizeBits))
  }

  def generateFromSymbols(
      symbols: List[DoldecompSymbol],
      headerRoots: List[Path],
      namespace: String = "doldecomp.generated",
      serviceName: String = "DolDecompApi",
      wordSizeBits: Int = 32
  ): Either[List[String], List[IrService]] = {
    val headerStructs = loadStructs(headerRoots)
    val objectSymbols = symbols.filter(_.symbolType.contains("object")).filter(_.cType.nonEmpty)

    if (objectSymbols.isEmpty) {
      Left(List("No object symbols with ctype metadata were found."))
    } else {
      val errors = mutable.ListBuffer.empty[String]
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
        val reachableStructs = collectReachableStructs(operations, headerStructs)
        val reachableByName = reachableStructs.toMap
        val builtStructs = mutable.Map.empty[String, IrType.MemoryMappedStruct]

        def buildStruct(name: String): IrType.MemoryMappedStruct = {
          builtStructs.getOrElseUpdate(
            name,
            IrType.MemoryMappedStruct(
              id = ShapeId.from(s"$namespace#$name"),
              members = CHeaderParser.extractFields(reachableByName(name)).map {
                case (fieldTypeName, declarator) =>
                  toIrMember(
                    namespace = namespace,
                    structName = name,
                    fieldTypeName = fieldTypeName,
                    fieldDeclarator = declarator,
                    reachableStructs = reachableByName,
                    buildStruct = buildStruct
                  )
              },
              declaredSizeBits = None
            )
          )
        }

        operations.foreach(operation => buildStruct(operation.rootStructName))

        val irOperations = operations.map { operation =>
          val outputShapeId = ShapeId.from(s"$namespace#${operation.outputName}")
          val rootStruct = buildStruct(operation.rootStructName)
          val outputMember = IrMember(
            id = ShapeId.from(s"$namespace#${operation.outputName}$$value"),
            name = "value",
            target = rootStruct,
            staticAddress = Some(operation.symbol.address),
            paddingRepeats = None,
            isPointer = false,
            isArray = false,
            arrayLength = None,
            endianOverride = None,
            primitiveOverride = None
          )

          IrOperation(
            name = operation.operationName,
            routePath = s"/$serviceName/${operation.operationName}",
            output = IrType.EnclosingStruct(
              id = outputShapeId,
              members = List(outputMember),
              declaredSizeBits = None
            )
          )
        }

        Right(
          List(
            IrService(
              name = serviceName,
              wordSizeBits = Some(wordSizeBits),
              defaultEndian = IrEndian.Big,
              operations = irOperations
            )
          )
        )
      }
    }
  }

  private def toIrMember(
      namespace: String,
      structName: String,
      fieldTypeName: String,
      fieldDeclarator: IASTDeclarator,
      reachableStructs: Map[String, IASTCompositeTypeSpecifier],
      buildStruct: String => IrType.MemoryMappedStruct
  ): IrMember = {
    val normalizedType = CHeaderParser.normalizeTypeName(fieldTypeName).replaceAll("\\s+", "")
    val fieldName = CHeaderParser.fieldName(fieldDeclarator)
    val memberName = toCamelCase(fieldName)
    val memberId = ShapeId.from(s"$namespace#$structName$$$memberName")
    val isPointer = CHeaderParser.pointerDepth(fieldDeclarator) > 0
    val arrayLength = CHeaderParser.arrayLength(fieldDeclarator)

    val resolvedTarget = if (isPointer) {
      IrType.Primitive(IrPrimitive.LongWord)
    } else {
      arrayLength match {
        case Some(_) =>
          val elementType = typeForName(normalizedType, reachableStructs, buildStruct)
          IrType.ListType(
            id = ShapeId.from(s"$namespace#${structName}${toPascalCase(fieldName)}Array"),
            element = elementType,
            bytesAlias = false,
            bitsAlias = false
          )
        case None =>
          typeForName(normalizedType, reachableStructs, buildStruct)
      }
    }
    val explicitPrimitive = primitiveForType(normalizedType)

    IrMember(
      id = memberId,
      name = memberName,
      target = resolvedTarget,
      staticAddress = None,
      paddingRepeats = None,
      isPointer = isPointer,
      isArray = arrayLength.nonEmpty,
      arrayLength = arrayLength,
      endianOverride = None,
      primitiveOverride =
        if (isPointer) None
        else explicitPrimitive.filter(isExplicitSizedPrimitive)
    )
  }

  private def isExplicitSizedPrimitive(kind: IrPrimitive): Boolean =
    kind match {
      case IrPrimitive.U8 | IrPrimitive.S8 | IrPrimitive.U16 | IrPrimitive.S16 | IrPrimitive.U32 |
          IrPrimitive.S32 | IrPrimitive.U64 | IrPrimitive.S64 | IrPrimitive.U128 |
          IrPrimitive.S128 | IrPrimitive.F8 | IrPrimitive.F16 | IrPrimitive.F32 | IrPrimitive.F64 |
          IrPrimitive.Char | IrPrimitive.Bool =>
        true
      case IrPrimitive.LongWord =>
        false
    }

  private def typeForName(
      normalizedType: String,
      reachableStructs: Map[String, IASTCompositeTypeSpecifier],
      buildStruct: String => IrType.MemoryMappedStruct
  ): IrType = {
    if (reachableStructs.contains(normalizedType)) {
      buildStruct(normalizedType)
    } else {
      IrType.Primitive(primitiveForType(normalizedType).getOrElse(IrPrimitive.LongWord))
    }
  }

  private def primitiveForType(typeName: String): Option[IrPrimitive] = {
    val normalized = CHeaderParser.normalizeTypeName(typeName).replaceAll("\\s+", "")
    normalized match {
      case "u8"    => Some(IrPrimitive.U8)
      case "s8"    => Some(IrPrimitive.S8)
      case "u16"   => Some(IrPrimitive.U16)
      case "s16"   => Some(IrPrimitive.S16)
      case "u32"   => Some(IrPrimitive.U32)
      case "s32"   => Some(IrPrimitive.S32)
      case "u64"   => Some(IrPrimitive.U64)
      case "s64"   => Some(IrPrimitive.S64)
      case "u128"  => Some(IrPrimitive.U128)
      case "s128"  => Some(IrPrimitive.S128)
      case "f32"   => Some(IrPrimitive.F32)
      case "f64"   => Some(IrPrimitive.F64)
      case "char"  => Some(IrPrimitive.Char)
      case "bool"  => Some(IrPrimitive.Bool)
      case "byte"  => Some(IrPrimitive.S8)
      case "short" => Some(IrPrimitive.S16)
      case "int"   => Some(IrPrimitive.S32)
      case "long"  => Some(IrPrimitive.LongWord)
      case _       => None
    }
  }

  private def loadStructs(headerRoots: List[Path]): Map[String, IASTCompositeTypeSpecifier] = {
    val headerFiles = headerRoots.flatMap(collectHeaderFiles).distinct
    headerFiles.flatMap { path =>
      val source = new String(Files.readAllBytes(path))
      CHeaderParser.parse(source)
    }.toMap
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

  private def collectReachableStructs(
      operations: List[OperationModel],
      headerStructs: Map[String, IASTCompositeTypeSpecifier]
  ): List[(String, IASTCompositeTypeSpecifier)] = {
    val visited = mutable.LinkedHashSet.empty[String]

    def visit(structName: String): Unit = {
      if (!visited.contains(structName)) {
        visited += structName
        headerStructs.get(structName).foreach { struct =>
          CHeaderParser.extractFields(struct).foreach { case (fieldTypeName, declarator) =>
            if (CHeaderParser.pointerDepth(declarator) == 0) {
              val normalized = CHeaderParser.normalizeTypeName(fieldTypeName).replaceAll("\\s+", "")
              if (headerStructs.contains(normalized)) {
                visit(normalized)
              }
            }
          }
        }
      }
    }

    operations.foreach(operation => visit(operation.rootStructName))
    visited.toList.flatMap(name => headerStructs.get(name).map(name -> _))
  }

  private def toPascalCase(value: String): String =
    value
      .split("[^A-Za-z0-9]+")
      .toList
      .filter(_.nonEmpty)
      .map(part => s"${part.head.toUpper}${part.drop(1)}")
      .mkString

  private def toCamelCase(value: String): String = {
    val pascal = toPascalCase(value)
    if (pascal.isEmpty) "value" else s"${pascal.head.toLower}${pascal.drop(1)}"
  }
}
