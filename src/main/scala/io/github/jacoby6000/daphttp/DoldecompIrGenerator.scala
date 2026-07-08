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
      rootTypeName: String,
      isArray: Boolean,
      arrayLength: Option[Int]
  )

  private final case class ResolvedSymbol(
      symbol: DoldecompSymbol,
      typeName: String,
      isArray: Boolean,
      arrayLength: Option[Int]
  )

  def generateFromPaths(
      symbolsPath: Path,
      headerRoots: List[Path],
      namespace: String = "doldecomp.generated",
      serviceName: String = "DolDecompApi",
      wordSizeBits: Int = 32
  ): Either[List[String], IrGenerationResult] = {
    val symbolsContent = new String(Files.readAllBytes(symbolsPath))
    DoldecompSymbolsParser
      .parse(symbolsContent)
      .map(generateFromSymbols(_, headerRoots, namespace, serviceName, wordSizeBits))
  }

  def generateFromSymbols(
      symbols: List[DoldecompSymbol],
      headerRoots: List[Path],
      namespace: String = "doldecomp.generated",
      serviceName: String = "DolDecompApi",
      wordSizeBits: Int = 32
  ): IrGenerationResult = {
    DapHttpLoggers.irSourceDoldecomp.info(
      "Generating IR from {} symbol(s) across {} header root(s)",
      Integer.valueOf(symbols.size),
      Integer.valueOf(headerRoots.size)
    )
    val headerStructs = loadStructs(headerRoots)
    val globalDeclarations = loadGlobalDeclarations(headerRoots)
    val fieldInitializerLengths = loadFieldInitializerLengths(headerRoots, headerStructs)
    val dataObjectSymbols =
      symbols.filter(_.symbolType.contains("object")).filter(_.section.contains(".data"))
    val resolvedSymbols = dataObjectSymbols.flatMap(resolveSymbol(_, globalDeclarations))
    val warnings = mutable.ListBuffer.empty[String]

    if (resolvedSymbols.isEmpty) {
      DapHttpLoggers.irSourceDoldecomp.warn(
        "No .data object symbols with a matching C declaration were found"
      )
      IrGenerationResult(
        warnings = List("No .data object symbols with a matching C declaration were found."),
        services = Nil
      )
    } else {
      val validResolved = resolvedSymbols.filter { resolved =>
        validateResolvedType(resolved.symbol.name, resolved.typeName, headerStructs) match {
          case Some(error) =>
            warnings += error
            DapHttpLoggers.irSourceDoldecomp.debug("Skipping symbol: {}", error)
            false
          case None =>
            true
        }
      }

      if (validResolved.isEmpty) {
        IrGenerationResult(warnings.toList, Nil)
      } else {
        val operations = validResolved.map { resolved =>
          OperationModel(
            symbol = resolved.symbol,
            operationName = s"Get${toPascalCase(resolved.symbol.name)}",
            outputName = s"Get${toPascalCase(resolved.symbol.name)}Output",
            rootTypeName = resolved.typeName,
            isArray = resolved.isArray,
            arrayLength = resolved.arrayLength
          )
        }

        val reachableStructs = collectReachableStructs(operations, headerStructs)
        val reachableByName = reachableStructs.toMap
        val builtStructs = mutable.Map.empty[String, IrType.MemoryMappedStruct]

        def buildStruct(name: String): IrType.MemoryMappedStruct = {
          builtStructs.getOrElseUpdate(
            name,
            reachableByName.get(name) match {
              case None =>
                throw new IllegalStateException(
                  s"Missing struct definition for '$name' while building IR."
                )
              case Some(composite) =>
                IrType.MemoryMappedStruct(
                  id = ShapeId.from(s"$namespace#$name"),
                  members =
                    CHeaderParser.extractFields(composite).map { case (fieldTypeName, declarator) =>
                      toIrMember(
                        namespace = namespace,
                        structName = name,
                        fieldTypeName = fieldTypeName,
                        fieldDeclarator = declarator,
                        reachableStructs = reachableByName,
                        buildStruct = buildStruct,
                        fieldInitializerLengths = fieldInitializerLengths
                      )
                    },
                  declaredSizeBits = None
                )
            }
          )
        }

        def isStructType(typeName: String): Boolean = reachableByName.contains(typeName)

        def rootElementIrType(typeName: String): IrType =
          if (isStructType(typeName)) {
            buildStruct(typeName)
          } else {
            IrType.Primitive(primitiveForType(typeName).getOrElse(IrPrimitive.LongWord))
          }

        def rootOutputIrType(operation: OperationModel): IrType =
          if (operation.isArray) {
            IrType.ListType(
              id = ShapeId.from(s"$namespace#${operation.outputName}ValueArray"),
              element = rootElementIrType(operation.rootTypeName),
              bytesAlias = false,
              bitsAlias = false
            )
          } else {
            rootElementIrType(operation.rootTypeName)
          }

        def elementSizeBytesForRootType(typeName: String): Option[Int] =
          if (isStructType(typeName)) {
            Some(irStructSizeBytes(buildStruct(typeName), wordSizeBits))
          } else {
            primitiveForType(typeName).flatMap { kind =>
              bitsForPrimitive(kind, Some(wordSizeBits))
                .map(bits => math.ceil(bits.toDouble / 8d).toInt)
            }
          }

        val operationsWithArrayLengths = operations.flatMap { operation =>
          if (operation.isArray && operation.arrayLength.isEmpty) {
            elementSizeBytesForRootType(operation.rootTypeName) match {
              case Some(elementSizeBytes) =>
                inferArrayLength(operation.symbol, elementSizeBytes) match {
                  case Some(length) =>
                    Some(operation.copy(arrayLength = Some(length)))
                  case None =>
                    warnings += s"${operation.symbol.name}: Unable to infer array length for '${operation.rootTypeName}'."
                    None
                }
              case None =>
                warnings += s"${operation.symbol.name}: Unable to determine element size for '${operation.rootTypeName}'."
                None
            }
          } else {
            Some(operation)
          }
        }

        operationsWithArrayLengths.foreach { operation =>
          if (isStructType(operation.rootTypeName)) {
            buildStruct(operation.rootTypeName)
          }
        }

        val irOperations = operationsWithArrayLengths.flatMap { operation =>
          try {
            val outputShapeId = ShapeId.from(s"$namespace#${operation.outputName}")
            val rootTarget = rootOutputIrType(operation)
            val outputMember = IrMember(
              id = ShapeId.from(s"$namespace#${operation.outputName}$$value"),
              name = "value",
              target = rootTarget,
              staticAddress = Some(operation.symbol.address),
              paddingRepeats = None,
              isPointer = false,
              isArray = operation.isArray,
              arrayLength = operation.arrayLength,
              endianOverride = None,
              primitiveOverride =
                if (operation.isArray || isStructType(operation.rootTypeName)) {
                  None
                } else {
                  primitiveForType(operation.rootTypeName).filter(isExplicitSizedPrimitive)
                },
              readSizeBytes = operation.symbol.sizeBytes
            )

            Some(
              IrOperation(
                name = operation.operationName,
                routePath = s"/$serviceName/${operation.operationName}",
                output = IrType.EnclosingStruct(
                  id = outputShapeId,
                  members = List(outputMember),
                  declaredSizeBits = None
                )
              )
            )
          } catch {
            case ex: Exception =>
              warnings += s"${operation.symbol.name}: ${ex.getMessage}"
              None
          }
        }

        val result = IrGenerationResult(
          warnings = warnings.toList,
          services = List(
            IrService(
              name = serviceName,
              wordSizeBits = Some(wordSizeBits),
              defaultEndian = IrEndian.Big,
              operations = irOperations
            )
          )
        )
        DapHttpLoggers.irSourceDoldecomp.info(
          "Generated {} operation(s) with {} warning(s) from {} resolved symbol(s)",
          Integer.valueOf(irOperations.size),
          Integer.valueOf(result.warnings.size),
          Integer.valueOf(resolvedSymbols.size)
        )
        irOperations.foreach { operation =>
          DapHttpLoggers.irSourceDoldecomp.debug("Operation {}", operation.routePath)
        }
        result
      }
    }
  }

  private def toIrMember(
      namespace: String,
      structName: String,
      fieldTypeName: String,
      fieldDeclarator: IASTDeclarator,
      reachableStructs: Map[String, IASTCompositeTypeSpecifier],
      buildStruct: String => IrType.MemoryMappedStruct,
      fieldInitializerLengths: Map[(String, String), Int]
  ): IrMember = {
    val normalizedType = CHeaderParser.normalizeTypeName(fieldTypeName).replaceAll("\\s+", "")
    val fieldName = CHeaderParser.fieldName(fieldDeclarator)
    val memberName = toCamelCase(fieldName)
    val memberId = ShapeId.from(s"$namespace#$structName$$$memberName")
    val isPointer = CHeaderParser.pointerDepth(fieldDeclarator) > 0
    val arrayLength =
      CHeaderParser
        .arrayLength(fieldDeclarator)
        .orElse(fieldInitializerLengths.get((structName, fieldName)))

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
    val charType = isCharType(normalizedType)

    IrMember(
      id = memberId,
      name = memberName,
      target = resolvedTarget,
      staticAddress = None,
      paddingRepeats = None,
      isPointer = isPointer,
      isArray = CHeaderParser.isArrayField(fieldDeclarator),
      arrayLength = arrayLength,
      endianOverride = None,
      primitiveOverride =
        if (isPointer && charType) {
          Some(IrPrimitive.Char)
        } else if (isPointer) {
          None
        } else {
          explicitPrimitive.filter(isExplicitSizedPrimitive)
        }
    )
  }

  private def isCharType(normalizedType: String): Boolean =
    normalizedType == "char"

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

  private def resolveSymbol(
      symbol: DoldecompSymbol,
      globalDeclarations: Map[String, GlobalVariableDeclaration]
  ): Option[ResolvedSymbol] =
    symbol.cType
      .map { explicitType =>
        ResolvedSymbol(
          symbol = symbol,
          typeName = explicitType,
          isArray = false,
          arrayLength = None
        )
      }
      .orElse {
        globalDeclarations.get(symbol.name).map { declaration =>
          ResolvedSymbol(
            symbol = symbol,
            typeName = declaration.typeName,
            isArray = declaration.isArray,
            arrayLength = declaration.resolvedArrayLength
          )
        }
      }

  private def validateResolvedType(
      symbolName: String,
      typeName: String,
      headerStructs: Map[String, IASTCompositeTypeSpecifier]
  ): Option[String] =
    if (headerStructs.contains(typeName) || primitiveForType(typeName).isDefined) {
      None
    } else {
      Some(s"$symbolName: Missing struct or primitive definition for resolved type '$typeName'.")
    }

  private def inferArrayLength(symbol: DoldecompSymbol, elementSizeBytes: Int): Option[Int] =
    symbol.sizeBytes.flatMap { totalSize =>
      def divided(size: Int): Option[Int] =
        Option.when(size > 0 && totalSize % size == 0)(totalSize / size)

      divided(elementSizeBytes).orElse {
        divided(alignTo(elementSizeBytes, 4)).orElse(divided(alignTo(elementSizeBytes, 8)))
      }
    }

  private def alignTo(size: Int, alignment: Int): Int =
    ((size + alignment - 1) / alignment) * alignment

  private def irStructSizeBytes(struct: IrType.MemoryMappedStruct, wordSizeBits: Int): Int = {
    val wordSize = Some(wordSizeBits)
    val memberBits = struct.members.flatMap(irMemberBitWidth(_, wordSize))
    if (memberBits.isEmpty) 0 else math.ceil(memberBits.sum.toDouble / 8d).toInt
  }

  private def irMemberBitWidth(member: IrMember, wordSize: Option[Int]): Option[Int] = {
    if (member.isPointer) {
      wordSize
    } else {
      member.primitiveOverride.flatMap(bitsForPrimitive(_, wordSize)).orElse {
        member.target match {
          case IrType.Primitive(kind)            => bitsForPrimitive(kind, wordSize)
          case listType: IrType.ListType         => irListBitWidth(member, listType, wordSize)
          case nested: IrType.MemoryMappedStruct =>
            Some(irStructSizeBytes(nested, wordSize.getOrElse(32)) * 8)
          case _ => None
        }
      }
    }
  }

  private def bitsForPrimitive(kind: IrPrimitive, wordSize: Option[Int]): Option[Int] =
    kind match {
      case IrPrimitive.Bool     => Some(1)
      case IrPrimitive.Char     => Some(8)
      case IrPrimitive.U8       => Some(8)
      case IrPrimitive.S8       => Some(8)
      case IrPrimitive.U16      => Some(16)
      case IrPrimitive.S16      => Some(16)
      case IrPrimitive.U32      => Some(32)
      case IrPrimitive.S32      => Some(32)
      case IrPrimitive.U64      => Some(64)
      case IrPrimitive.S64      => Some(64)
      case IrPrimitive.U128     => Some(128)
      case IrPrimitive.S128     => Some(128)
      case IrPrimitive.F8       => Some(8)
      case IrPrimitive.F16      => Some(16)
      case IrPrimitive.F32      => Some(32)
      case IrPrimitive.F64      => Some(64)
      case IrPrimitive.LongWord => wordSize.orElse(Some(64))
    }

  private def irListBitWidth(
      member: IrMember,
      listType: IrType.ListType,
      wordSize: Option[Int]
  ): Option[Int] =
    member.arrayLength.flatMap { length =>
      listType.element match {
        case IrType.Primitive(kind) =>
          bitsForPrimitive(kind, wordSize).map(_ * length)
        case nested: IrType.MemoryMappedStruct =>
          Some(irStructSizeBytes(nested, wordSize.getOrElse(32)) * 8 * length)
        case _ => None
      }
    }

  private def loadGlobalDeclarations(
      headerRoots: List[Path]
  ): Map[String, GlobalVariableDeclaration] = {
    val sourceFiles = headerRoots.flatMap(collectSourceFiles).distinct
    sourceFiles
      .flatMap { path =>
        val source = new String(Files.readAllBytes(path))
        CHeaderParser.parseGlobalDeclarations(source)
      }
      .groupBy(_.name)
      .view
      .mapValues(mergeGlobalDeclarations)
      .toMap
  }

  private def mergeGlobalDeclarations(
      declarations: List[GlobalVariableDeclaration]
  ): GlobalVariableDeclaration = {
    val primary = declarations.head
    GlobalVariableDeclaration(
      name = primary.name,
      typeName = declarations.map(_.typeName).find(_.nonEmpty).getOrElse(primary.typeName),
      isArray = declarations.exists(_.isArray),
      declaratorLength = declarations.flatMap(_.declaratorLength).headOption,
      initializerLength = declarations.flatMap(_.initializerLength).headOption
    )
  }

  private def loadFieldInitializerLengths(
      headerRoots: List[Path],
      headerStructs: Map[String, IASTCompositeTypeSpecifier]
  ): Map[(String, String), Int] = {
    val sourceFiles = headerRoots.flatMap(collectSourceFiles).distinct
    sourceFiles
      .flatMap { path =>
        val source = new String(Files.readAllBytes(path))
        CHeaderParser.parseStructFieldInitializerLengths(source, headerStructs).toList
      }
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).max)
      .toMap
  }

  private def loadStructs(headerRoots: List[Path]): Map[String, IASTCompositeTypeSpecifier] = {
    val sourceFiles = headerRoots.flatMap(collectSourceFiles).distinct
    sourceFiles.flatMap { path =>
      val source = new String(Files.readAllBytes(path))
      CHeaderParser.parse(source)
    }.toMap
  }

  private def collectSourceFiles(root: Path): List[Path] = {
    if (!Files.exists(root)) {
      Nil
    } else if (Files.isRegularFile(root) && isSourceFile(root)) {
      List(root)
    } else if (Files.isDirectory(root)) {
      val stream = Files.walk(root)
      try {
        stream
          .iterator()
          .asScala
          .filter(path => Files.isRegularFile(path) && isSourceFile(path))
          .toList
      } finally {
        stream.close()
      }
    } else {
      Nil
    }
  }

  private def isSourceFile(path: Path): Boolean = {
    val name = path.toString
    name.endsWith(".h") || name.endsWith(".c")
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

    operations.foreach { operation =>
      if (headerStructs.contains(operation.rootTypeName)) {
        visit(operation.rootTypeName)
      }
    }
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
