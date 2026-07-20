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
      routePath: String,
      rootTypeName: String,
      isArray: Boolean,
      arrayLength: Option[Int],
      pointerDepth: Int
  )

  private final case class ResolvedSymbol(
      symbol: DoldecompSymbol,
      typeName: String,
      isArray: Boolean,
      arrayLength: Option[Int],
      pointerDepth: Int
  )

  private final case class MergedNamedMap[A](
      values: Map[String, A],
      warnings: List[String]
  )

  def generateFromPaths(
      symbolsPath: Path,
      headerRoots: List[Path],
      namespace: String = "doldecomp.generated",
      serviceName: String = "DolDecompApi",
      wordSizeBits: Int = 32,
      extraDataSections: Set[String] = Set.empty
  ): Either[List[String], IrGenerationResult] = {
    val symbolsContent = new String(Files.readAllBytes(symbolsPath))
    DoldecompSymbolsParser
      .parse(symbolsContent)
      .map(
        generateFromSymbols(_, headerRoots, namespace, serviceName, wordSizeBits, extraDataSections)
      )
  }

  def generateFromSymbols(
      symbols: List[DoldecompSymbol],
      headerRoots: List[Path],
      namespace: String = "doldecomp.generated",
      serviceName: String = "DolDecompApi",
      wordSizeBits: Int = 32,
      extraDataSections: Set[String] = Set.empty
  ): IrGenerationResult = {
    val effectiveHeaderRoots = expandHeaderRoots(headerRoots)
    DapHttpLoggers.irSourceDoldecomp.info(
      "Generating IR from {} symbol(s) across {} header root(s)",
      Integer.valueOf(symbols.size),
      Integer.valueOf(effectiveHeaderRoots.size)
    )
    val macroLoad = loadAllMacros(effectiveHeaderRoots)
    val allMacros = CHeaderParser.builtInMacros ++ macroLoad.values
    // Enums before structs: array bounds like jobjs[HUD_PLACE_MAX] need enumerator values in
    // ScannerInfo. Count-sentinel iteration stays enum-only; full enumerator macros are for
    // structs/globals only (injecting them during enum reparse collides with tag redefinitions).
    val enumParse = loadEnums(effectiveHeaderRoots, allMacros)
    val enums = enumParse.enums
    val macrosWithEnums = allMacros ++ enumeratorMacros(enums).filter { case (name, _) =>
      !allMacros.contains(name)
    }
    val structLoad = loadStructs(effectiveHeaderRoots, macrosWithEnums)
    val headerStructs = structLoad.values
    val structMemberOffsets = loadStructMemberOffsets(effectiveHeaderRoots)
    val globalDeclarations = loadGlobalDeclarations(effectiveHeaderRoots, macrosWithEnums)
    val fieldInitializerLengths =
      loadFieldInitializerLengths(effectiveHeaderRoots, headerStructs, macrosWithEnums)
    val typedefLoad = loadTypedefs(effectiveHeaderRoots, macrosWithEnums)
    val typedefs = typedefLoad.values
    val sectionResult = SectionFilter.filterDataSymbols(symbols, extraDataSections)
    sectionResult.warnings.foreach(w => DapHttpLoggers.irSourceDoldecomp.warn("{}", w))
    val dataObjectSymbols = sectionResult.dataSymbols
    val warnings = mutable.ListBuffer.empty[String]
    warnings ++= sectionResult.warnings
    warnings ++= macroLoad.warnings
    warnings ++= structLoad.warnings
    warnings ++= typedefLoad.warnings
    warnings ++= enumParse.warnings
    (macroLoad.warnings ++ structLoad.warnings ++ typedefLoad.warnings ++ enumParse.warnings)
      .foreach(w => DapHttpLoggers.irSourceDoldecomp.warn("{}", w))

    val unresolvedSymbols = mutable.ListBuffer.empty[String]
    val resolvedSymbols = dataObjectSymbols.flatMap { symbol =>
      resolveSymbol(symbol, globalDeclarations) match {
        case some @ Some(_) =>
          some
        case None =>
          unresolvedSymbols += symbol.name
          None
      }
    }
    if (unresolvedSymbols.nonEmpty) {
      val names = unresolvedSymbols.take(40).mkString(", ")
      val suffix =
        if (unresolvedSymbols.size > 40) s", … (${unresolvedSymbols.size - 40} more)" else ""
      val message =
        s"Skipping ${unresolvedSymbols.size} object symbol(s) with no ctype and no matching global C declaration under --headers: $names$suffix."
      warnings += message
      DapHttpLoggers.irSourceDoldecomp.warn("{}", message)
    }

    if (resolvedSymbols.isEmpty) {
      if (!warnings.exists(_.contains("matching global C declaration"))) {
        val message = "No data object symbols with a matching C declaration were found."
        warnings += message
        DapHttpLoggers.irSourceDoldecomp.warn("{}", message)
      }
      IrGenerationResult(warnings = warnings.toList.distinct, services = Nil)
    } else {
      val missingTypeByName = mutable.LinkedHashMap.empty[String, mutable.ListBuffer[String]]
      val validResolved = resolvedSymbols.filter { resolved =>
        validateResolvedType(
          resolved.symbol.name,
          resolved.typeName,
          headerStructs,
          typedefs,
          enums,
          allMacros
        ) match {
          case Some(error) =>
            if (error.contains("Missing struct or primitive definition")) {
              val typeKey =
                CHeaderParser.normalizeTypeName(resolved.typeName).replaceAll("\\s+", "")
              missingTypeByName
                .getOrElseUpdate(typeKey, mutable.ListBuffer.empty)
                .append(resolved.symbol.name)
            } else {
              warnings += error
              DapHttpLoggers.irSourceDoldecomp.debug("Skipping symbol: {}", error)
            }
            false
          case None =>
            true
        }
      }
      missingTypeByName.foreach { case (typeName, symbols) =>
        val names = symbols.take(20).mkString(", ")
        val suffix = if (symbols.size > 20) s", … (${symbols.size - 20} more)" else ""
        val message =
          s"Skipping ${symbols.size} symbol(s) with missing struct or primitive definition for '$typeName' (add the defining headers via --headers): $names$suffix."
        warnings += message
        DapHttpLoggers.irSourceDoldecomp.warn("{}", message)
      }

      if (validResolved.isEmpty) {
        IrGenerationResult(warnings.toList, Nil)
      } else {
        val usedOutputNames = mutable.Set.empty[String]
        val usedOperationNames = mutable.Set.empty[String]
        val usedRoutePaths = mutable.Set.empty[String]
        val operations = validResolved.map { resolved =>
          val pascalName = toPascalCase(resolved.symbol.name)
          val baseRoutePath = ApiRoutes.normalize(s"/$serviceName/${resolved.symbol.name}")
          val routePath =
            if (usedRoutePaths.contains(baseRoutePath)) {
              var suffix = 2
              while (
                usedRoutePaths.contains(
                  ApiRoutes.normalize(s"/$serviceName/${resolved.symbol.name}_$suffix")
                )
              )
                suffix += 1
              val result = ApiRoutes.normalize(s"/$serviceName/${resolved.symbol.name}_$suffix")
              usedRoutePaths += result
              result
            } else {
              usedRoutePaths += baseRoutePath
              baseRoutePath
            }
          val baseOutputName = s"${pascalName}Output"
          val outputName =
            if (usedOutputNames.contains(baseOutputName)) {
              var suffix = 2
              while (usedOutputNames.contains(s"$baseOutputName$suffix")) suffix += 1
              val result = s"$baseOutputName$suffix"
              usedOutputNames += result
              result
            } else {
              usedOutputNames += baseOutputName
              baseOutputName
            }
          val baseOperationName = s"Get$pascalName"
          val operationName =
            if (usedOperationNames.contains(baseOperationName)) {
              var suffix = 2
              while (usedOperationNames.contains(s"$baseOperationName$suffix")) suffix += 1
              val result = s"$baseOperationName$suffix"
              usedOperationNames += result
              result
            } else {
              usedOperationNames += baseOperationName
              baseOperationName
            }
          OperationModel(
            symbol = resolved.symbol,
            operationName = operationName,
            outputName = outputName,
            routePath = routePath,
            rootTypeName = resolved.typeName,
            isArray = resolved.isArray,
            arrayLength = resolved.arrayLength,
            pointerDepth = resolved.pointerDepth
          )
        }

        val reachableStructs = collectReachableStructs(operations, headerStructs, typedefs)
        val reachableByName = reachableStructs.toMap
        val builtStructs = mutable.Map.empty[String, IrType.MemoryMappedStruct]
        val buildingStructs = mutable.Set.empty[String]
        val builtEnums = mutable.Map.empty[String, IrType.IntEnum]

        def buildEnum(name: String): IrType.IntEnum =
          builtEnums.getOrElseUpdate(
            name, {
              val definition = enums.getOrElse(
                name,
                throw new IllegalStateException(
                  s"Missing enum definition for '$name' while building IR."
                )
              )
              IrType.IntEnum(
                id = ShapeId.from(s"$namespace#${definition.name}"),
                values = definition.values,
                underlying = IrPrimitive.S32
              )
            }
          )

        def buildStruct(name: String): IrType.MemoryMappedStruct = {
          builtStructs.getOrElseUpdate(
            name,
            reachableByName.get(name) match {
              case None =>
                throw new IllegalStateException(
                  s"Missing struct definition for '$name' while building IR."
                )
              case Some(composite) =>
                buildingStructs += name
                try {
                  IrType.MemoryMappedStruct(
                    id = ShapeId.from(s"$namespace#$name"),
                    members = buildStructMembers(
                      namespace = namespace,
                      structName = name,
                      wordSizeBits = wordSizeBits,
                      fields = enrichFieldOffsets(
                        name,
                        CHeaderParser.extractFields(composite),
                        structMemberOffsets
                      ),
                      reachableStructs = reachableByName,
                      buildStruct = buildStruct,
                      buildEnum = buildEnum,
                      enums = enums,
                      pointeeTypeFor = pointeeTypeFor,
                      fieldInitializerLengths = fieldInitializerLengths,
                      typedefs = typedefs
                    ),
                    declaredSizeBits = None
                  )
                } finally {
                  buildingStructs -= name
                }
            }
          )
        }

        def pointeeTypeFor(name: String, structs: Map[String, IASTCompositeTypeSpecifier]): IrType =
          if (structs.contains(name) && buildingStructs.contains(name))
            IrType.Ref(ShapeId.from(s"$namespace#$name"))
          else
            typeForName(name, structs, buildStruct, buildEnum, enums, typedefs)

        def isStructType(typeName: String): Boolean =
          reachableByName.contains(typeName) || reachableByName.contains(
            resolveTypedef(typeName, typedefs)
          )

        def isEnumType(typeName: String): Boolean = {
          val normalized = CHeaderParser.normalizeTypeName(typeName).replaceAll("\\s+", "")
          enums.contains(normalized) || enums.contains(resolveTypedef(typeName, typedefs))
        }

        def resolveEnumName(typeName: String): Option[String] = {
          val normalized = CHeaderParser.normalizeTypeName(typeName).replaceAll("\\s+", "")
          if (enums.contains(normalized)) Some(normalized)
          else {
            val resolved = resolveTypedef(typeName, typedefs)
            if (enums.contains(resolved)) Some(resolved) else None
          }
        }

        def rootElementIrType(typeName: String, pointerDepth: Int): IrType =
          if (pointerDepth > 0) {
            IrType.Primitive(IrPrimitive.LongWord)
          } else {
            if (reachableByName.contains(typeName)) {
              buildStruct(typeName)
            } else {
              val resolved = resolveTypedef(typeName, typedefs)
              if (reachableByName.contains(resolved)) {
                buildStruct(resolved)
              } else {
                resolveEnumName(typeName)
                  .orElse(resolveEnumName(resolved))
                  .map(buildEnum)
                  .getOrElse(
                    IrType.Primitive(primitiveForType(resolved).getOrElse(IrPrimitive.LongWord))
                  )
              }
            }
          }

        def rootOutputIrType(operation: OperationModel): IrType =
          if (operation.isArray) {
            IrType.ListType(
              id = ShapeId.from(s"$namespace#${operation.outputName}ValueArray"),
              element = rootElementIrType(operation.rootTypeName, operation.pointerDepth),
              bytesAlias = false,
              bitsAlias = false
            )
          } else {
            rootElementIrType(operation.rootTypeName, operation.pointerDepth)
          }

        def elementSizeBytesForRootType(typeName: String, pointerDepth: Int): Option[Int] =
          if (pointerDepth > 0) {
            Some(wordSizeBits / 8)
          } else {
            val resolved = resolveTypedef(typeName, typedefs)
            if (isStructType(resolved)) {
              Some(irStructSizeBytes(buildStruct(resolved), wordSizeBits))
            } else if (isEnumType(resolved) || isEnumType(typeName)) {
              bitsForPrimitive(IrPrimitive.S32, Some(wordSizeBits))
                .map(bits => math.ceil(bits.toDouble / 8d).toInt)
            } else {
              primitiveForType(resolved).flatMap { kind =>
                bitsForPrimitive(kind, Some(wordSizeBits))
                  .map(bits => math.ceil(bits.toDouble / 8d).toInt)
              }
            }
          }

        val operationsWithArrayLengths = operations.flatMap { operation =>
          if (operation.isArray && operation.arrayLength.isEmpty) {
            val resolvedRoot = resolveTypedef(operation.rootTypeName, typedefs)
            if (operation.pointerDepth == 0 && isStructType(resolvedRoot)) {
              warnings +=
                s"${operation.symbol.name}: Unable to infer array length for aggregate '${operation.rootTypeName}' without a C declarator bound or initializer; symbol size may include element-stride padding."
              None
            } else {
              elementSizeBytesForRootType(operation.rootTypeName, operation.pointerDepth) match {
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
            }
          } else {
            Some(operation)
          }
        }

        operationsWithArrayLengths.foreach { operation =>
          if (operation.isArray) {
            for {
              length <- operation.arrayLength
              totalSize <- operation.symbol.sizeBytes
              elementSize <- elementSizeBytesForRootType(
                operation.rootTypeName,
                operation.pointerDepth
              )
              // Warn only when the symbol is too small for a packed array. Larger sizes may be
              // per-element stride padding (when divisible) or trailing section padding.
              if length > 0 && totalSize < length.toLong * elementSize
            } {
              warnings +=
                s"${operation.symbol.name}: symbol size 0x${totalSize.toHexString} is inconsistent with array length $length and element size $elementSize."
            }
          }
        }

        operationsWithArrayLengths.foreach { operation =>
          val resolvedRoot = resolveTypedef(operation.rootTypeName, typedefs)
          if (operation.pointerDepth == 0 && isStructType(resolvedRoot)) {
            buildStruct(resolvedRoot)
          } else if (operation.pointerDepth > 0 && isStructType(resolvedRoot)) {
            buildStruct(resolvedRoot)
          } else {
            resolveEnumName(operation.rootTypeName).orElse(resolveEnumName(resolvedRoot)).foreach {
              buildEnum
            }
          }
        }

        val irOperations = operationsWithArrayLengths.flatMap { operation =>
          try {
            val outputShapeId = ShapeId.from(s"$namespace#${operation.outputName}")
            val rootTarget = rootOutputIrType(operation)
            val resolvedRoot = resolveTypedef(operation.rootTypeName, typedefs)
            val outputMember = IrMember(
              id = ShapeId.from(s"$namespace#${operation.outputName}$$value"),
              name = "value",
              target = rootTarget,
              staticAddress = Some(operation.symbol.address),
              paddingRepeats = None,
              isPointer = operation.pointerDepth > 0,
              isArray = operation.isArray,
              arrayLength = operation.arrayLength,
              endianOverride = None,
              primitiveOverride =
                if (
                  operation.pointerDepth > 0 && isCharType(
                    CHeaderParser.normalizeTypeName(operation.rootTypeName).replaceAll("\\s+", "")
                  )
                ) {
                  Some(IrPrimitive.Char)
                } else if (
                  operation.pointerDepth > 0 || operation.isArray || isStructType(
                    operation.rootTypeName
                  ) || isEnumType(operation.rootTypeName) || isEnumType(resolvedRoot)
                ) {
                  None
                } else {
                  primitiveForType(resolvedRoot)
                    .filter(isExplicitSizedPrimitive)
                    .orElse(wordSizePrimitive(wordSizeBits))
                },
              readSizeBytes = operation.symbol.sizeBytes
            )

            Some(
              IrOperation(
                name = operation.operationName,
                routePath = operation.routePath,
                output = IrType.EnclosingStruct(
                  id = outputShapeId,
                  members = List(outputMember),
                  declaredSizeBits = None
                ),
                pointerChain =
                  if (operation.pointerDepth > 0) {
                    val normalizedRootType =
                      CHeaderParser.normalizeTypeName(operation.rootTypeName).replaceAll("\\s+", "")
                    Some(
                      IrPointerChain(
                        pointeeType = rootElementIrType(operation.rootTypeName, pointerDepth = 0),
                        pointerDepth = operation.pointerDepth,
                        outerArrayLength = if (operation.isArray) operation.arrayLength else None,
                        followCString = isCharType(normalizedRootType)
                      )
                    )
                  } else {
                    None
                  }
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

  private sealed trait StructFieldGroup
  private final case class RegularFieldGroup(field: StructFieldDecl) extends StructFieldGroup
  private final case class BitmaskFieldGroup(
      name: String,
      fields: List[StructFieldDecl],
      storageBits: Int
  ) extends StructFieldGroup

  private def buildStructMembers(
      namespace: String,
      structName: String,
      wordSizeBits: Int,
      fields: List[StructFieldDecl],
      reachableStructs: Map[String, IASTCompositeTypeSpecifier],
      buildStruct: String => IrType.MemoryMappedStruct,
      buildEnum: String => IrType.IntEnum,
      enums: Map[String, CEnumDefinition],
      pointeeTypeFor: (String, Map[String, IASTCompositeTypeSpecifier]) => IrType,
      fieldInitializerLengths: Map[(String, String), Int],
      typedefs: Map[String, String]
  ): List[IrMember] =
    groupBitfieldFields(fields, wordSizeBits).flatMap {
      case RegularFieldGroup(field) =>
        List(
          toIrMember(
            namespace = namespace,
            structName = structName,
            fieldTypeName = field.typeName,
            fieldDeclarator = field.declarator,
            unionGroup = field.unionGroup,
            offsetBytes = field.offsetBytes,
            wordSizeBits = wordSizeBits,
            reachableStructs = reachableStructs,
            buildStruct = buildStruct,
            buildEnum = buildEnum,
            enums = enums,
            pointeeTypeFor = pointeeTypeFor,
            fieldInitializerLengths = fieldInitializerLengths,
            typedefs = typedefs
          )
        )
      case BitmaskFieldGroup(name, bitfields, storageBits) if bitfields.size == 1 =>
        val field = bitfields.head
        List(
          IrMember(
            id = ShapeId.from(
              s"$namespace#$structName$$${toCamelCase(CHeaderParser.fieldName(field.declarator))}"
            ),
            name = toCamelCase(CHeaderParser.fieldName(field.declarator)),
            target = IrType.Primitive(IrPrimitive.Bool),
            staticAddress = None,
            paddingRepeats = None,
            isPointer = false,
            isArray = false,
            arrayLength = None,
            endianOverride = None,
            primitiveOverride = None,
            layoutBitWidth = Some(storageBits),
            offsetBytes = field.offsetBytes
          )
        )
      case BitmaskFieldGroup(name, bitfields, storageBits) =>
        List(
          toBitmaskMember(
            namespace = namespace,
            structName = structName,
            memberName = name,
            bitfields = bitfields,
            storageBits = storageBits,
            offsetBytes = bitfields.flatMap(_.offsetBytes).headOption
          )
        )
    }

  private def enrichFieldOffsets(
      structName: String,
      fields: List[StructFieldDecl],
      offsets: Map[(String, String), Int]
  ): List[StructFieldDecl] =
    fields.map { field =>
      val fieldName = CHeaderParser.fieldName(field.declarator)
      field.copy(offsetBytes = offsets.get((structName, fieldName)).orElse(field.offsetBytes))
    }

  private def groupBitfieldFields(
      fields: List[StructFieldDecl],
      wordSizeBits: Int
  ): List[StructFieldGroup] = {
    val groups = mutable.ListBuffer.empty[StructFieldGroup]
    val pending = mutable.ListBuffer.empty[StructFieldDecl]
    var pendingType: Option[String] = None
    var pendingUsedBits = 0
    var pendingTypeBits = 0

    def flushPending(): Unit =
      if (pending.nonEmpty) {
        val bitfields = pending.toList
        groups += BitmaskFieldGroup(
          name = bitmaskGroupName(bitfields),
          fields = bitfields,
          storageBits = pendingTypeBits
        )
        pending.clear()
        pendingType = None
        pendingUsedBits = 0
        pendingTypeBits = 0
      }

    fields.foreach { field =>
      field.bitFieldWidth match {
        case None =>
          flushPending()
          groups += RegularFieldGroup(field)
        case Some(width) =>
          val normalizedType =
            CHeaderParser.normalizeTypeName(field.typeName).replaceAll("\\s+", "")
          val typeBits = primitiveStorageBits(normalizedType, wordSizeBits).getOrElse(8)
          val sameType = pendingType.forall(_ == normalizedType)
          if (pending.nonEmpty && sameType && pendingUsedBits + width <= typeBits) {
            pending += field
            pendingUsedBits += width
          } else {
            flushPending()
            pendingType = Some(normalizedType)
            pendingTypeBits = typeBits
            pending += field
            pendingUsedBits = width
          }
      }
    }
    flushPending()
    groups.toList
  }

  private def bitmaskGroupName(fields: List[StructFieldDecl]): String = {
    val names = fields.map(field => CHeaderParser.fieldName(field.declarator))
    val commonPrefix = names
      .reduceLeftOption { (left, right) =>
        left.zip(right).takeWhile(Function.tupled(_ == _)).map(_._1).mkString
      }
      .getOrElse("")
    val trimmed =
      if (commonPrefix.nonEmpty && commonPrefix.last == '_') commonPrefix.dropRight(1)
      else if (commonPrefix.nonEmpty) commonPrefix
      else names.headOption.getOrElse("bits")
    toCamelCase(trimmed)
  }

  private def primitiveStorageBits(normalizedType: String, wordSizeBits: Int): Option[Int] =
    primitiveForType(normalizedType).flatMap(bitsForPrimitive(_, Some(wordSizeBits)))

  private def toBitmaskMember(
      namespace: String,
      structName: String,
      memberName: String,
      bitfields: List[StructFieldDecl],
      storageBits: Int,
      offsetBytes: Option[Int]
  ): IrMember = {
    val bitmaskId = ShapeId.from(s"$namespace#$structName${toPascalCase(memberName)}Bits")
    IrMember(
      id = ShapeId.from(s"$namespace#$structName$$$memberName"),
      name = memberName,
      target = IrType.Bitmask(
        id = bitmaskId,
        members = bitfields.map { field =>
          val fieldName = CHeaderParser.fieldName(field.declarator)
          IrMember(
            id = ShapeId.from(
              s"$namespace#${structName}${toPascalCase(memberName)}${toPascalCase(fieldName)}"
            ),
            name = toCamelCase(fieldName),
            target = IrType.Primitive(IrPrimitive.Bool),
            staticAddress = None,
            paddingRepeats = None,
            isPointer = false,
            isArray = false,
            arrayLength = None,
            endianOverride = None,
            primitiveOverride = None
          )
        },
        declaredSizeBits = Some(storageBits)
      ),
      staticAddress = None,
      paddingRepeats = None,
      isPointer = false,
      isArray = false,
      arrayLength = None,
      endianOverride = None,
      primitiveOverride = None,
      offsetBytes = offsetBytes
    )
  }

  private def toIrMember(
      namespace: String,
      structName: String,
      fieldTypeName: String,
      fieldDeclarator: IASTDeclarator,
      unionGroup: Option[String],
      offsetBytes: Option[Int],
      wordSizeBits: Int,
      reachableStructs: Map[String, IASTCompositeTypeSpecifier],
      buildStruct: String => IrType.MemoryMappedStruct,
      buildEnum: String => IrType.IntEnum,
      enums: Map[String, CEnumDefinition],
      pointeeTypeFor: (String, Map[String, IASTCompositeTypeSpecifier]) => IrType,
      fieldInitializerLengths: Map[(String, String), Int],
      typedefs: Map[String, String]
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

    val funcPointerSig =
      if (isPointer) CHeaderParser.extractFunctionPointerSignature(fieldDeclarator, fieldTypeName)
      else None

    lazy val pointeeType = pointeeTypeFor(normalizedType, reachableStructs)
    val resolvedTarget = if (funcPointerSig.isDefined && arrayLength.isEmpty) {
      IrType.FunctionPointer(
        name = toPascalCase(fieldName),
        params = funcPointerSig.get.params.map(p => FunctionPointerParam(p.typeName, p.name)),
        returnType = funcPointerSig.get.returnType
      )
    } else if (isPointer && arrayLength.isDefined && arrayLength.exists(_ > 0)) {
      IrType.ListType(
        id = ShapeId.from(s"$namespace#${structName}${toPascalCase(fieldName)}Array"),
        element = pointeeType,
        bytesAlias = false,
        bitsAlias = false
      )
    } else if (isPointer) {
      IrType.Primitive(IrPrimitive.LongWord)
    } else {
      arrayLength match {
        case Some(_) =>
          val elementType =
            typeForName(normalizedType, reachableStructs, buildStruct, buildEnum, enums, typedefs)
          IrType.ListType(
            id = ShapeId.from(s"$namespace#${structName}${toPascalCase(fieldName)}Array"),
            element = elementType,
            bytesAlias = false,
            bitsAlias = false
          )
        case None =>
          typeForName(normalizedType, reachableStructs, buildStruct, buildEnum, enums, typedefs)
      }
    }
    // DESNOTE(jbarber, 2026-07-20): Resolve typedefs (e.g. enum_t → int, MessageBufferID → int)
    // before choosing primitiveOverride. typeForName already resolves for the target shape; without
    // the same step here, IrSizingWarnings treats S32/F32 targets as ambiguous Integer/Float.
    val resolvedTypeName = resolveTypedef(normalizedType, typedefs)
    val explicitPrimitive = primitiveForType(resolvedTypeName)
    val charType = isCharType(normalizedType) || isCharType(resolvedTypeName)

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
        if (isPointer && charType && funcPointerSig.isEmpty) {
          Some(IrPrimitive.Char)
        } else if (isPointer && funcPointerSig.isEmpty) {
          None
        } else if (isPointer) {
          None
        } else {
          explicitPrimitive.filter(isExplicitSizedPrimitive).orElse {
            if (resolvedTarget == IrType.Primitive(IrPrimitive.LongWord))
              wordSizePrimitive(wordSizeBits)
            else None
          }
        },
      unionGroup = unionGroup,
      offsetBytes = offsetBytes
    )
  }

  private def isCharType(normalizedType: String): Boolean = {
    val base = normalizedType.replaceAll("\\s+", " ").stripSuffix(" const").trim
    base == "char" || base == "unsigned char" || base == "signed char"
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

  private def wordSizePrimitive(wordSizeBits: Int): Option[IrPrimitive] =
    wordSizeBits match {
      case 8   => Some(IrPrimitive.U8)
      case 16  => Some(IrPrimitive.U16)
      case 32  => Some(IrPrimitive.U32)
      case 64  => Some(IrPrimitive.U64)
      case 128 => Some(IrPrimitive.U128)
      case _   => None
    }

  private def typeForName(
      normalizedType: String,
      reachableStructs: Map[String, IASTCompositeTypeSpecifier],
      buildStruct: String => IrType.MemoryMappedStruct,
      buildEnum: String => IrType.IntEnum,
      enums: Map[String, CEnumDefinition],
      typedefs: Map[String, String]
  ): IrType = {
    val resolved = resolveTypedef(normalizedType, typedefs)
    if (reachableStructs.contains(normalizedType)) {
      buildStruct(normalizedType)
    } else if (reachableStructs.contains(resolved)) {
      buildStruct(resolved)
    } else if (enums.contains(normalizedType)) {
      buildEnum(normalizedType)
    } else if (enums.contains(resolved)) {
      buildEnum(resolved)
    } else {
      IrType.Primitive(primitiveForType(resolved).getOrElse(IrPrimitive.LongWord))
    }
  }

  private def primitiveForType(typeName: String): Option[IrPrimitive] = {
    val normalized = CHeaderParser.normalizeTypeName(typeName).replaceAll("\\s+", "")
    normalized match {
      case "u8"                          => Some(IrPrimitive.U8)
      case "s8"                          => Some(IrPrimitive.S8)
      case "u16"                         => Some(IrPrimitive.U16)
      case "s16"                         => Some(IrPrimitive.S16)
      case "u32"                         => Some(IrPrimitive.U32)
      case "s32"                         => Some(IrPrimitive.S32)
      case "u64"                         => Some(IrPrimitive.U64)
      case "s64"                         => Some(IrPrimitive.S64)
      case "u128"                        => Some(IrPrimitive.U128)
      case "s128"                        => Some(IrPrimitive.S128)
      case "f32"                         => Some(IrPrimitive.F32)
      case "f64"                         => Some(IrPrimitive.F64)
      case "char"                        => Some(IrPrimitive.Char)
      case "bool"                        => Some(IrPrimitive.Bool)
      case "byte"                        => Some(IrPrimitive.S8)
      case "short"                       => Some(IrPrimitive.S16)
      case "int"                         => Some(IrPrimitive.S32)
      case "long"                        => Some(IrPrimitive.LongWord)
      case "float"                       => Some(IrPrimitive.F32)
      case "double"                      => Some(IrPrimitive.F64)
      case "unsignedchar"                => Some(IrPrimitive.U8)
      case "signedchar"                  => Some(IrPrimitive.S8)
      case "unsignedshort"               => Some(IrPrimitive.U16)
      case "signedshort"                 => Some(IrPrimitive.S16)
      case "unsignedint"                 => Some(IrPrimitive.U32)
      case "signedint"                   => Some(IrPrimitive.S32)
      case "unsigned"                    => Some(IrPrimitive.U32)
      case "signed"                      => Some(IrPrimitive.S32)
      case "unsignedlong"                => Some(IrPrimitive.LongWord)
      case "signedlong"                  => Some(IrPrimitive.LongWord)
      case "longlong"                    => Some(IrPrimitive.S64)
      case "unsignedlonglong"            => Some(IrPrimitive.U64)
      case "signedlonglong"              => Some(IrPrimitive.S64)
      case "unsignedshortint"            => Some(IrPrimitive.U16)
      case "signedshortint"              => Some(IrPrimitive.S16)
      case "unsignedlongint"             => Some(IrPrimitive.LongWord)
      case "signedlongint"               => Some(IrPrimitive.LongWord)
      case "longlongint"                 => Some(IrPrimitive.S64)
      case "unsignedlonglongint"         => Some(IrPrimitive.U64)
      case "signedlonglongint"           => Some(IrPrimitive.S64)
      case "void"                        => Some(IrPrimitive.LongWord)
      case _ if normalized.endsWith("*") =>
        Some(IrPrimitive.LongWord)
      case _ => None
    }
  }

  private def resolveSymbol(
      symbol: DoldecompSymbol,
      globalDeclarations: Map[String, GlobalVariableDeclaration]
  ): Option[ResolvedSymbol] = {
    val declaration = globalDeclarations.get(symbol.name)
    symbol.cType
      .map { explicitType =>
        // DESNOTE(jbarber, 2026-07-19): ctype names the element/pointee type, but array length
        // and pointer depth still come from the matching C global declaration when present.
        val normalized = CHeaderParser.normalizeTypeName(explicitType).replaceAll("\\s+", "")
        val pointerFromCtype = normalized.count(_ == '*')
        val baseType = normalized.replaceAll("\\*", "")
        ResolvedSymbol(
          symbol = symbol,
          typeName = baseType,
          isArray = declaration.exists(_.isArray),
          arrayLength = declaration.flatMap(_.resolvedArrayLength),
          pointerDepth = declaration.map(_.pointerDepth).getOrElse(pointerFromCtype)
        )
      }
      .orElse {
        declaration.map { decl =>
          ResolvedSymbol(
            symbol = symbol,
            typeName = decl.typeName,
            isArray = decl.isArray,
            arrayLength = decl.resolvedArrayLength,
            pointerDepth = decl.pointerDepth
          )
        }
      }
  }

  private def validateResolvedType(
      symbolName: String,
      typeName: String,
      headerStructs: Map[String, IASTCompositeTypeSpecifier],
      typedefs: Map[String, String],
      enums: Map[String, CEnumDefinition],
      macros: Map[String, String]
  ): Option[String] = {
    val normalized = CHeaderParser.normalizeTypeName(typeName).replaceAll("\\s+", "")
    if (
      headerStructs.contains(normalized) || enums.contains(normalized) || primitiveForType(
        normalized
      ).isDefined
    ) {
      None
    } else {
      val resolved = resolveTypeName(typeName, typedefs, macros)
      if (
        headerStructs.contains(resolved) || enums.contains(resolved) || primitiveForType(
          resolved
        ).isDefined
      ) {
        None
      } else {
        Some(s"$symbolName: Missing struct or primitive definition for resolved type '$typeName'.")
      }
    }
  }

  private def resolveTypeName(
      typeName: String,
      typedefs: Map[String, String],
      macros: Map[String, String]
  ): String = {
    val viaTypedef = resolveTypedef(typeName, typedefs)
    if (primitiveForType(viaTypedef).isDefined) {
      viaTypedef
    } else {
      val normalized = CHeaderParser.normalizeTypeName(viaTypedef).replaceAll("\\s+", "")
      val base = normalized.replaceAll("\\*", "").trim
      val pointerCount = normalized.count(_ == '*')
      macros.get(base) match {
        case Some(expansion) =>
          // DESNOTE(jbarber, 2026-07-20): Melee uses macros as opaque types (UNK_T → void*).
          val expanded = resolveTypedef(expansion, typedefs)
          val expandedNorm = CHeaderParser.normalizeTypeName(expanded).replaceAll("\\s+", "")
          val extraPointers = (0 until pointerCount).map(_ => "*").mkString
          s"$expandedNorm$extraPointers".replaceAll("\\s+", "")
        case None =>
          viaTypedef
      }
    }
  }

  private def resolveTypedef(typeName: String, typedefs: Map[String, String]): String = {
    val normalized = CHeaderParser.normalizeTypeName(typeName).replaceAll("\\s+", "")
    val base = normalized.replaceAll("\\*", "").trim
    val pointerCount = normalized.count(_ == '*')
    val resolvedBase = resolveTypedefChain(base, typedefs, Set.empty)
    val pointerSuffix = (0 until pointerCount).map(_ => "*").mkString
    s"$resolvedBase$pointerSuffix".replaceAll("\\s+", "")
  }

  private def resolveTypedefChain(
      name: String,
      typedefs: Map[String, String],
      visited: Set[String]
  ): String = {
    if (visited.contains(name)) name
    else {
      typedefs.get(name) match {
        case Some(resolved) =>
          val cleanResolved = CHeaderParser.normalizeTypeName(resolved).replaceAll("\\s+", "")
          val cleanBase = cleanResolved.replaceAll("\\*", "").trim
          if (cleanBase != name) resolveTypedefChain(cleanBase, typedefs, visited + name)
          else cleanResolved
        case None =>
          name
      }
    }
  }

  private def inferArrayLength(symbol: DoldecompSymbol, elementSizeBytes: Int): Option[Int] =
    symbol.sizeBytes.flatMap { totalSize =>
      Option.when(elementSizeBytes > 0 && totalSize % elementSizeBytes == 0)(
        totalSize / elementSizeBytes
      )
    }

  private def irStructSizeBytes(struct: IrType.MemoryMappedStruct, wordSizeBits: Int): Int = {
    val wordSize = Some(wordSizeBits)
    if (struct.members.exists(_.offsetBytes.isDefined)) {
      struct.members
        .flatMap { member =>
          for {
            offset <- member.offsetBytes
            sizeBytes <- irMemberSizeBytes(member, wordSize)
          } yield offset + sizeBytes
        }
        .maxOption
        .getOrElse(0)
    } else {
      val memberBits = struct.members.flatMap(irMemberBitWidth(_, wordSize))
      if (memberBits.isEmpty) 0 else math.ceil(memberBits.sum.toDouble / 8d).toInt
    }
  }

  private def irMemberSizeBytes(member: IrMember, wordSize: Option[Int]): Option[Int] =
    member.readSizeBytes.orElse {
      irMemberBitWidth(member, wordSize).map(bits => math.ceil(bits.toDouble / 8d).toInt)
    }

  private def irMemberBitWidth(member: IrMember, wordSize: Option[Int]): Option[Int] = {
    member.layoutBitWidth.orElse {
      if (member.isPointer) {
        wordSize
      } else {
        member.primitiveOverride.flatMap(bitsForPrimitive(_, wordSize)).orElse {
          member.target match {
            case IrType.Primitive(kind)            => bitsForPrimitive(kind, wordSize)
            case _: IrType.IntEnum                 => bitsForPrimitive(IrPrimitive.S32, wordSize)
            case listType: IrType.ListType         => irListBitWidth(member, listType, wordSize)
            case nested: IrType.MemoryMappedStruct =>
              Some(irStructSizeBytes(nested, wordSize.getOrElse(32)) * 8)
            case bitmask: IrType.Bitmask =>
              bitmask.declaredSizeBits.orElse {
                Some(bitmask.members.map(_ => 1).sum)
              }
            case _ => None
          }
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
        case _: IrType.IntEnum =>
          bitsForPrimitive(IrPrimitive.S32, wordSize).map(_ * length)
        case nested: IrType.MemoryMappedStruct =>
          Some(irStructSizeBytes(nested, wordSize.getOrElse(32)) * 8 * length)
        case _ => None
      }
    }

  private def loadStructMemberOffsets(headerRoots: List[Path]): Map[(String, String), Int] = {
    val sourceFiles = headerRoots.flatMap(collectSourceFiles).distinct
    sourceFiles.flatMap { path =>
      val source = new String(Files.readAllBytes(path))
      CHeaderOffsetParser.parse(source).toList
    }.toMap
  }

  private def loadGlobalDeclarations(
      headerRoots: List[Path],
      macros: Map[String, String]
  ): Map[String, GlobalVariableDeclaration] = {
    val sourceFiles = headerRoots.flatMap(collectSourceFiles).distinct
    sourceFiles
      .flatMap { path =>
        val source = new String(Files.readAllBytes(path))
        CHeaderParser.parseGlobalDeclarations(source, macros)
      }
      .groupBy(_.name)
      .view
      .mapValues(mergeGlobalDeclarations)
      .toMap
  }

  private[daphttp] def mergeGlobalDeclarations(
      declarations: List[GlobalVariableDeclaration]
  ): GlobalVariableDeclaration = {
    // DESNOTE(jbarber, 2026-07-19): Files.walk order is not stable across environments, so never
    // take "first declaration wins" for lengths/pointer depth. Prefer non-static definitions with
    // explicit array metadata; array-ness and pointerDepth come from that primary so a mismatched
    // forward declaration cannot change the preferred definition's route shape.
    def preference(d: GlobalVariableDeclaration): (Boolean, Boolean, Boolean, Boolean, String) =
      (
        !d.isStatic,
        d.declaratorLength.isDefined,
        d.initializerLength.isDefined,
        d.typeName.nonEmpty,
        d.typeName
      )
    val ordered = declarations.sortBy(preference)(
      Ordering
        .Tuple5(
          Ordering.Boolean,
          Ordering.Boolean,
          Ordering.Boolean,
          Ordering.Boolean,
          Ordering.String
        )
        .reverse
    )
    val primary = ordered.head
    val compatibleArrayDeclarations =
      if (primary.isArray) ordered.filter(d => d.isArray && d.pointerDepth == primary.pointerDepth)
      else Nil
    GlobalVariableDeclaration(
      name = primary.name,
      typeName = ordered.map(_.typeName).find(_.nonEmpty).getOrElse(primary.typeName),
      isArray = primary.isArray,
      declaratorLength = compatibleArrayDeclarations.flatMap(_.declaratorLength).headOption,
      initializerLength = compatibleArrayDeclarations.flatMap(_.initializerLength).headOption,
      pointerDepth = primary.pointerDepth,
      isStatic = primary.isStatic
    )
  }

  private def loadFieldInitializerLengths(
      headerRoots: List[Path],
      headerStructs: Map[String, IASTCompositeTypeSpecifier],
      macros: Map[String, String]
  ): Map[(String, String), Int] = {
    val sourceFiles = headerRoots.flatMap(collectSourceFiles).distinct
    sourceFiles
      .flatMap { path =>
        val source = new String(Files.readAllBytes(path))
        CHeaderParser.parseStructFieldInitializerLengths(source, headerStructs, macros).toList
      }
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).max)
      .toMap
  }

  private def loadAllMacros(headerRoots: List[Path]): MergedNamedMap[String] = {
    val sourceFiles = headerRoots.flatMap(collectSourceFiles).distinct
    val entries = sourceFiles.flatMap { path =>
      val source = new String(Files.readAllBytes(path))
      CHeaderParser.extractMacros(source).toList
    }
    mergeNamedEntries(entries, "macro", _ == _)
  }

  private def loadStructs(
      headerRoots: List[Path],
      macros: Map[String, String]
  ): MergedNamedMap[IASTCompositeTypeSpecifier] = {
    val sourceFiles = headerRoots.flatMap(collectSourceFiles).distinct
    val entries = sourceFiles.flatMap { path =>
      val source = new String(Files.readAllBytes(path))
      CHeaderParser.parse(source, macros).toList
    }
    mergeNamedEntries(entries, "struct", structDefinitionsEquivalent)
  }

  private def loadTypedefs(
      headerRoots: List[Path],
      macros: Map[String, String]
  ): MergedNamedMap[String] = {
    val sourceFiles = headerRoots.flatMap(collectSourceFiles).distinct
    val entries = sourceFiles.flatMap { path =>
      val source = new String(Files.readAllBytes(path))
      CHeaderParser.parseTypedefs(source, macros).toList
    }
    mergeNamedEntries(entries, "typedef", _ == _)
  }

  private def mergeNamedEntries[A](
      entries: List[(String, A)],
      kind: String,
      equivalent: (A, A) => Boolean
  ): MergedNamedMap[A] = {
    val conflictNames = mutable.LinkedHashSet.empty[String]
    val merged = mutable.LinkedHashMap.empty[String, A]
    entries.foreach { case (name, value) =>
      merged.get(name) match {
        case None =>
          merged(name) = value
        case Some(existing) if equivalent(existing, value) =>
          ()
        case Some(_) =>
          conflictNames += name
      }
    }
    MergedNamedMap(merged.toMap, summarizeNameConflicts(kind, conflictNames.toList))
  }

  private def summarizeNameConflicts(kind: String, names: List[String]): List[String] =
    if (names.isEmpty) {
      Nil
    } else {
      val sample = names.take(20).mkString(", ")
      val suffix = if (names.size > 20) s", … (${names.size - 20} more)" else ""
      List(
        s"Conflicting $kind definitions for ${names.size} name(s); keeping first: $sample$suffix."
      )
    }

  private def structDefinitionsEquivalent(
      left: IASTCompositeTypeSpecifier,
      right: IASTCompositeTypeSpecifier
  ): Boolean =
    structFieldSignature(left) == structFieldSignature(right)

  private def structFieldSignature(composite: IASTCompositeTypeSpecifier): List[String] =
    CHeaderParser.extractFields(composite).map { field =>
      val fieldName =
        Option(field.declarator.getName).map(_.toString).getOrElse("")
      s"${field.typeName}|$fieldName|${field.bitFieldWidth}|${field.unionGroup}|${field.offsetBytes}"
    }

  private def loadEnums(
      headerRoots: List[Path],
      macros: Map[String, String]
  ): EnumParseResult = {
    val sourceFiles = headerRoots.flatMap(collectSourceFiles).distinct
    // DESNOTE(jbarber, 2026-07-20): Character motion enums form a dependency chain
    // (ftCo_MS_Count → ftMh_MS_Count → ftCh_MS_Count). Iterate: parse → harvest Count sentinels that
    // appear before any failed initializer in their enum → reparse until macros stabilize (capped).
    // Only Count sentinels are injected — exporting every enumerator as a macro would collide with
    // later redefinitions of the same enum tag (see enum-merge fixture).
    // See https://github.com/eclipse-cdt/cdt/blob/main/core/org.eclipse.cdt.core/parser/org/eclipse/cdt/internal/core/dom/parser/ValueFactory.java
    def parseAll(macrosForPass: Map[String, String]): List[EnumParseResult] =
      sourceFiles.map { path =>
        val source = new String(Files.readAllBytes(path))
        CHeaderParser.parseEnums(source, macrosForPass)
      }

    var macrosForPass = macros
    var results = parseAll(macrosForPass)
    var pass = 0
    var continue = true
    while (continue && pass < 4) {
      pass += 1
      val warningCount = results.map(_.warnings.size).sum
      // Accumulate onto macrosForPass — never rebuild from the original `macros` alone, or a later
      // harvest that omits an earlier Count (because that enumerator name is now a macro in its
      // defining file) would drop dependencies needed by consumers.
      val nextMacros = macrosForPass ++ countSentinelMacros(results)
      if (nextMacros == macrosForPass || warningCount == 0) {
        continue = false
      } else {
        macrosForPass = nextMacros
        results = parseAll(macrosForPass)
      }
    }
    mergeEnumParseResults(results)
  }

  private def countSentinelMacros(results: List[EnumParseResult]): Map[String, String] = {
    // DESNOTE(jbarber, 2026-07-20): Only export Count sentinels that appear before any failed
    // initializer in the same enum. Masterhand's ftMh_MS_Count is valid even when SelfCount fails
    // on a missing ftCo_MS_Count; Captain's Count is not, because an earlier initializer failed.
    val failedKeys = results
      .flatMap(_.warnings)
      .flatMap(warning => warning.split(':').headOption.map(_.trim).filter(_.nonEmpty))
      .toSet
    results
      .flatMap(_.enums.toList)
      .flatMap { case (enumName, definition) =>
        var seenFailure = false
        definition.values.flatMap { value =>
          if (failedKeys.contains(s"$enumName.${value.name}")) {
            seenFailure = true
          }
          if (
            !seenFailure && (value.name.endsWith("_Count") || value.name.endsWith("_SelfCount"))
          ) {
            Some(value.name -> value.value.toString)
          } else {
            None
          }
        }
      }
      .toMap
  }

  private def enumeratorMacros(enums: Map[String, CEnumDefinition]): Map[String, String] = {
    // DESNOTE(jbarber, 2026-07-20): After enums are merged, export every enumerator as a ScannerInfo
    // macro for struct/global parses so bounds like jobjs[HUD_PLACE_MAX] evaluate. First name wins
    // on cross-enum collisions. Do not feed these into loadEnums (see countSentinelMacros DESNOTE).
    enums.values
      .flatMap(_.values)
      .foldLeft(Map.empty[String, String]) { (acc, value) =>
        if (acc.contains(value.name)) acc else acc + (value.name -> value.value.toString)
      }
  }

  private def mergeEnumParseResults(results: List[EnumParseResult]): EnumParseResult = {
    val warnings = mutable.ListBuffer.empty[String]
    warnings ++= results.flatMap(_.warnings)
    val conflictNames = mutable.LinkedHashSet.empty[String]
    val merged = mutable.LinkedHashMap.empty[String, CEnumDefinition]
    results.foreach { result =>
      result.enums.foreach { case (name, definition) =>
        merged.get(name) match {
          case None =>
            merged(name) = definition
          case Some(existing) if existing.values == definition.values =>
            ()
          case Some(existing) if existing.values.isEmpty && definition.values.nonEmpty =>
            merged(name) = definition
          case Some(existing) if existing.values.nonEmpty && definition.values.isEmpty =>
            warnings += s"$name: Ignoring empty enum redefinition."
          case Some(_) =>
            conflictNames += name
        }
      }
    }
    warnings ++= summarizeNameConflicts("enum", conflictNames.toList)
    EnumParseResult(merged.toMap, warnings.toList)
  }

  private def expandHeaderRoots(headerRoots: List[Path]): List[Path] = {
    // DESNOTE(jbarber, 2026-07-20): Melee layouts put Dolphin SDK headers at
    // <repo>/extern/dolphin/include next to <repo>/src. Auto-append when present so Vec3/GXColor
    // resolve without a second --headers. Explicit --headers paths still win for ordering.
    // See https://github.com/zeldaret/melee (extern/dolphin).
    val extras = headerRoots.flatMap { root =>
      val absolute = root.toAbsolutePath.normalize
      Option(absolute.getParent)
        .map(_.resolve("extern/dolphin/include"))
        .filter(p => Files.isDirectory(p) && !headerRoots.exists(_.toAbsolutePath.normalize == p))
        .toList
    }.distinct
    if (extras.nonEmpty) {
      DapHttpLoggers.irSourceDoldecomp.info(
        "Also scanning adjacent Dolphin SDK include(s): {}",
        extras.map(_.toString).mkString(", ")
      )
    }
    (headerRoots ++ extras).map(_.toAbsolutePath.normalize).distinct
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
          .sortBy(_.toString)
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
      headerStructs: Map[String, IASTCompositeTypeSpecifier],
      typedefs: Map[String, String]
  ): List[(String, IASTCompositeTypeSpecifier)] = {
    val visited = mutable.LinkedHashSet.empty[String]

    def resolve(name: String): String = {
      val normalized = CHeaderParser.normalizeTypeName(name).replaceAll("\\s+", "")
      if (headerStructs.contains(normalized)) normalized
      else {
        val resolved = resolveTypedef(name, typedefs)
        if (headerStructs.contains(resolved)) resolved else normalized
      }
    }

    def visit(structName: String): Unit = {
      val resolved = resolve(structName)
      if (!visited.contains(resolved)) {
        visited += resolved
        headerStructs.get(resolved).foreach { struct =>
          CHeaderParser.extractFields(struct).foreach { field =>
            val normalized = CHeaderParser.normalizeTypeName(field.typeName).replaceAll("\\s+", "")
            val fieldResolved = resolveTypedef(field.typeName, typedefs)
            if (headerStructs.contains(normalized)) {
              visit(normalized)
            } else if (headerStructs.contains(fieldResolved)) {
              visit(fieldResolved)
            }
          }
        }
      }
    }

    operations.foreach { operation =>
      visit(operation.rootTypeName)
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
    val camel = if (pascal.isEmpty) "value" else s"${pascal.head.toLower}${pascal.drop(1)}"
    if (camel.head.isDigit) s"_$camel" else camel
  }
}
