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
    DapHttpLoggers.irSourceDoldecomp.info(
      "Generating IR from {} symbol(s) across {} header root(s)",
      Integer.valueOf(symbols.size),
      Integer.valueOf(headerRoots.size)
    )
    val allMacros = loadAllMacros(headerRoots)
    val headerStructs = loadStructs(headerRoots, allMacros)
    val structMemberOffsets = loadStructMemberOffsets(headerRoots)
    val globalDeclarations = loadGlobalDeclarations(headerRoots, allMacros)
    val fieldInitializerLengths = loadFieldInitializerLengths(headerRoots, headerStructs, allMacros)
    val typedefs = loadTypedefs(headerRoots, allMacros)
    val enumParse = loadEnums(headerRoots, allMacros)
    val enums = enumParse.enums
    val sectionResult = SectionFilter.filterDataSymbols(symbols, extraDataSections)
    sectionResult.warnings.foreach(w => DapHttpLoggers.irSourceDoldecomp.warn("{}", w))
    val dataObjectSymbols = sectionResult.dataSymbols
    val resolvedSymbols = dataObjectSymbols.flatMap(resolveSymbol(_, globalDeclarations))
    val warnings = mutable.ListBuffer.empty[String]
    warnings ++= sectionResult.warnings
    warnings ++= enumParse.warnings
    enumParse.warnings.foreach(w => DapHttpLoggers.irSourceDoldecomp.warn("{}", w))

    if (resolvedSymbols.isEmpty) {
      DapHttpLoggers.irSourceDoldecomp.warn(
        "No data object symbols with a matching C declaration were found"
      )
      IrGenerationResult(
        warnings = List("No data object symbols with a matching C declaration were found."),
        services = Nil
      )
    } else {
      val validResolved = resolvedSymbols.filter { resolved =>
        validateResolvedType(
          resolved.symbol.name,
          resolved.typeName,
          headerStructs,
          typedefs,
          enums
        ) match {
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
          } else {
            Some(operation)
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
    groupBitfieldFields(fields).flatMap {
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

  private def groupBitfieldFields(fields: List[StructFieldDecl]): List[StructFieldGroup] = {
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
          val typeBits = primitiveStorageBits(normalizedType).getOrElse(8)
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

  private def primitiveStorageBits(normalizedType: String): Option[Int] =
    primitiveForType(normalizedType).flatMap(bitsForPrimitive(_, Some(32)))

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
  ): Option[ResolvedSymbol] =
    symbol.cType
      .map { explicitType =>
        ResolvedSymbol(
          symbol = symbol,
          typeName = explicitType,
          isArray = false,
          arrayLength = None,
          pointerDepth = 0
        )
      }
      .orElse {
        globalDeclarations.get(symbol.name).map { declaration =>
          ResolvedSymbol(
            symbol = symbol,
            typeName = declaration.typeName,
            isArray = declaration.isArray,
            arrayLength = declaration.resolvedArrayLength,
            pointerDepth = declaration.pointerDepth
          )
        }
      }

  private def validateResolvedType(
      symbolName: String,
      typeName: String,
      headerStructs: Map[String, IASTCompositeTypeSpecifier],
      typedefs: Map[String, String],
      enums: Map[String, CEnumDefinition]
  ): Option[String] = {
    val normalized = CHeaderParser.normalizeTypeName(typeName).replaceAll("\\s+", "")
    if (
      headerStructs.contains(normalized) || enums.contains(normalized) || primitiveForType(
        normalized
      ).isDefined
    ) {
      None
    } else {
      val resolved = resolveTypedef(typeName, typedefs)
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
    if (struct.members.exists(_.offsetBytes.isDefined)) {
      struct.members
        .flatMap { member =>
          for {
            offset <- member.offsetBytes.orElse(Some(0))
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

  private def mergeGlobalDeclarations(
      declarations: List[GlobalVariableDeclaration]
  ): GlobalVariableDeclaration = {
    val primary = declarations.head
    GlobalVariableDeclaration(
      name = primary.name,
      typeName = declarations.map(_.typeName).find(_.nonEmpty).getOrElse(primary.typeName),
      isArray = declarations.exists(_.isArray),
      declaratorLength = declarations.flatMap(_.declaratorLength).headOption,
      initializerLength = declarations.flatMap(_.initializerLength).headOption,
      pointerDepth = declarations.map(_.pointerDepth).max
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

  private def loadAllMacros(headerRoots: List[Path]): Map[String, String] = {
    val sourceFiles = headerRoots.flatMap(collectSourceFiles).distinct
    sourceFiles.flatMap { path =>
      val source = new String(Files.readAllBytes(path))
      CHeaderParser.extractMacros(source).toList
    }.toMap
  }

  private def loadStructs(
      headerRoots: List[Path],
      macros: Map[String, String]
  ): Map[String, IASTCompositeTypeSpecifier] = {
    val sourceFiles = headerRoots.flatMap(collectSourceFiles).distinct
    sourceFiles.flatMap { path =>
      val source = new String(Files.readAllBytes(path))
      CHeaderParser.parse(source, macros)
    }.toMap
  }

  private def loadTypedefs(
      headerRoots: List[Path],
      macros: Map[String, String]
  ): Map[String, String] = {
    val sourceFiles = headerRoots.flatMap(collectSourceFiles).distinct
    sourceFiles.flatMap { path =>
      val source = new String(Files.readAllBytes(path))
      CHeaderParser.parseTypedefs(source, macros).toList
    }.toMap
  }

  private def loadEnums(
      headerRoots: List[Path],
      macros: Map[String, String]
  ): EnumParseResult = {
    val sourceFiles = headerRoots.flatMap(collectSourceFiles).distinct
    val results = sourceFiles.map { path =>
      val source = new String(Files.readAllBytes(path))
      CHeaderParser.parseEnums(source, macros)
    }
    mergeEnumParseResults(results)
  }

  private def mergeEnumParseResults(results: List[EnumParseResult]): EnumParseResult = {
    val warnings = mutable.ListBuffer.empty[String]
    warnings ++= results.flatMap(_.warnings)
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
          case Some(existing) =>
            warnings +=
              s"$name: Conflicting enum definitions; keeping first (${existing.values.size} values vs ${definition.values.size})."
        }
      }
    }
    EnumParseResult(merged.toMap, warnings.toList)
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
