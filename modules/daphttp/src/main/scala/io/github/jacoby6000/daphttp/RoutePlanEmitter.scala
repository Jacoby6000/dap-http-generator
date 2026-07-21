package io.github.jacoby6000.daphttp

import scala.collection.mutable.ListBuffer

/** IR → HTTP route plans (reads, pointer chains, member sub-routes). */
object RoutePlanEmitter {
  def emitRoutePlansFromIr(
      irServices: List[IrService]
  ): RoutePlansLoadResult = {
    val errors = ListBuffer.empty[String]
    val routes = ListBuffer.empty[(String, RoutePlan)]

    irServices.foreach { service =>
      service.wordSizeBits match {
        case None =>
          errors += s"${service.name}: Services must declare @wordSize."
          DapHttpLoggers.irEmit.warn("{}: Services must declare @wordSize.", service.name)
        case Some(wordSizeBits) =>
          service.operations.foreach { operation =>
            // DESNOTE(jbarber, 2026-07-18): All generated data routes live under /api so the
            // HTML UI and meta endpoints (/health, /routes, /dap-proxy) can share the same host
            // without colliding. Normalize here so hand-built IR in tests stays consistent
            // even if a generator forgot ApiRoutes.normalize.
            val httpPath = ApiRoutes.normalize(operation.routePath)
            val operationErrors = ListBuffer.empty[String]
            val reads = collectReadsForType(
              operation.output,
              None,
              httpPath,
              service.defaultEndian,
              Some(wordSizeBits),
              operationErrors
            )
            if (operationErrors.nonEmpty) {
              operationErrors.foreach(error =>
                DapHttpLoggers.irEmit.warn("{}: {}", httpPath, error)
              )
              errors ++= operationErrors.toList
            } else {
              val pointerChainPlan = buildPointerChainPlan(
                operation = operation,
                defaultEndian = service.defaultEndian,
                wordSizeBits = wordSizeBits,
                errors = operationErrors
              )
              if (operationErrors.nonEmpty) {
                operationErrors.foreach(error =>
                  DapHttpLoggers.irEmit.warn("{}: {}", httpPath, error)
                )
                errors ++= operationErrors.toList
              } else {
                val memberSubRoutes = buildMemberSubRoutes(
                  reads = reads,
                  defaultEndian = service.defaultEndian,
                  wordSizeBits = wordSizeBits,
                  errors = operationErrors
                )
                if (operationErrors.nonEmpty) {
                  operationErrors.foreach(error =>
                    DapHttpLoggers.irEmit.warn("{}: {}", httpPath, error)
                  )
                  errors ++= operationErrors.toList
                } else {
                  DapHttpLoggers.irEmit.debug(
                    "Compiled route {} with {} read(s)",
                    httpPath,
                    Integer.valueOf(reads.size)
                  )
                  routes += httpPath -> RoutePlan(
                    httpPath,
                    reads,
                    pointerChainPlan,
                    memberSubRoutes
                  )
                }
              }
            }
          }
      }
    }

    val result =
      RoutePlansLoadResult(routes.toMap, errors.toList.distinct, irServices)
    DapHttpLoggers.irEmit.info(
      "Emitted {} route(s) with {} error(s)",
      Integer.valueOf(result.routes.size),
      Integer.valueOf(result.errors.size)
    )
    result
  }

  /** Compile a JSON decode codec for an IR type (used by type overlays). */
  private[daphttp] def buildPointerChainPlan(
      operation: IrOperation,
      defaultEndian: IrEndian,
      wordSizeBits: Int,
      errors: ListBuffer[String]
  ): Option[PointerChainPlan] =
    operation.pointerChain.flatMap { chain =>
      operation.output.members
        .collectFirst {
          case member if member.staticAddress.isDefined =>
            val context = s"${operation.routePath}/chain"
            val resolvedPointeeSizeBytes =
              IrJsonCodecs.pointeeSizeBytes(chain.pointeeType, Some(wordSizeBits), errors)
            val pointeeDecodeCodec =
              IrJsonCodecs.compileJsonCodecForType(
                chain.pointeeType,
                defaultEndian,
                Some(wordSizeBits),
                errors,
                context
              )
            (
              member.staticAddress.get,
              resolvedPointeeSizeBytes,
              pointeeDecodeCodec,
              IrJsonCodecs.arrayElementStrideBytes(member, Some(wordSizeBits))
            )
        }
        .flatMap { case (baseAddress, resolvedSizeBytes, codecOpt, outerStride) =>
          for {
            sizeBytes <- resolvedSizeBytes
            codec <- codecOpt
          } yield PointerChainPlan(
            pointeeType = chain.pointeeType,
            pointerDepth = chain.pointerDepth,
            outerArrayLength = chain.outerArrayLength,
            baseAddress = baseAddress,
            endian = defaultEndian,
            wordSizeBits = wordSizeBits,
            pointeeSizeBytes = sizeBytes,
            pointeeDecodeCodec = Some(codec),
            followCString =
              chain.followCString || chain.pointeeType == IrType.Primitive(IrPrimitive.Char),
            outerElementStrideBytes = outerStride
          )
        }
    }

  private[daphttp] def buildMemberSubRoutes(
      reads: List[ReadPlan],
      defaultEndian: IrEndian,
      wordSizeBits: Int,
      errors: ListBuffer[String]
  ): List[MemberSubRoute] =
    reads.flatMap { readPlan =>
      readPlan.decodeType match {
        case Some(struct: IrType.Struct) =>
          buildSubRoutesForStruct(struct, readPlan.address, defaultEndian, wordSizeBits, errors)
        case Some(list: IrType.ListType) =>
          buildRootArraySubRoute(readPlan, list, wordSizeBits, errors).toList
        case _ =>
          Nil
      }
    }

  /** Indexed element routes for a root-level array read (`$basePath/0`, `$basePath/1`, …). */
  private[daphttp] def buildRootArraySubRoute(
      readPlan: ReadPlan,
      list: IrType.ListType,
      wordSizeBits: Int,
      errors: ListBuffer[String]
  ): Option[MemberSubRoute.ValueSubRoute] = {
    val endian = readPlan.endian
    val elemSize = IrJsonCodecs.pointeeSizeBytes(list.element, Some(wordSizeBits), errors)
    val stride = readPlan.elementStrideBytes.orElse(elemSize)
    val length = readPlan.arrayLength.orElse {
      for {
        size <- elemSize.orElse(stride)
        if size > 0 && readPlan.sizeBytes % size == 0
      } yield readPlan.sizeBytes / size
    }
    elemSize.filter(_ > 0).map { size =>
      MemberSubRoute.ValueSubRoute(
        memberName = MemberSubRoute.RootArrayMemberName,
        baseAddress = readPlan.address,
        memberOffsetBytes = 0,
        isArray = true,
        arrayLength = length,
        wordSizeBits = wordSizeBits,
        endian = endian,
        valueType = Some(list.element),
        elementSizeBytes = Some(size),
        elementStrideBytes = stride.orElse(Some(size)),
        decodeCodec = IrJsonCodecs.compileJsonCodecForType(
          list.element,
          endian,
          Some(wordSizeBits),
          errors,
          s"${readPlan.path}/[element]"
        )
      )
    }
  }

  private[daphttp] def buildSubRoutesForStruct(
      struct: IrType.Struct,
      baseAddress: Long,
      endian: IrEndian,
      wordSizeBits: Int,
      errors: ListBuffer[String]
  ): List[MemberSubRoute] = {
    val offsets = IrJsonCodecs.computeMemberOffsets(struct, Some(wordSizeBits), errors)
    struct.members.flatMap { member =>
      val memberOffset = offsets.getOrElse(member.name, 0)
      val context = s"subroute/${member.name}"
      val isFuncPointer = member.target.isInstanceOf[IrType.FunctionPointer]
      if (member.isPointer && !isFuncPointer) {
        buildPointerSubRoute(
          member,
          memberOffset,
          baseAddress,
          endian,
          wordSizeBits,
          errors,
          context
        )
      } else {
        buildValueSubRoute(member, memberOffset, baseAddress, endian, wordSizeBits, errors, context)
      }
    }
  }

  private[daphttp] def buildPointerSubRoute(
      member: IrMember,
      memberOffset: Int,
      baseAddress: Long,
      endian: IrEndian,
      wordSizeBits: Int,
      errors: ListBuffer[String],
      context: String
  ): Option[MemberSubRoute.PointerSubRoute] = {
    val pointeeType = member.target match {
      case _: IrType.FunctionPointer =>
        None
      case listType: IrType.ListType =>
        listType.element match {
          case _: IrType.FunctionPointer => None
          case element                   => Some(element)
        }
      case other if member.isPointer =>
        Some(other)
      case _ =>
        None
    }
    pointeeType.map { ptype =>
      val isCharPointee = ptype == IrType.Primitive(IrPrimitive.Char) ||
        member.primitiveOverride.contains(IrPrimitive.Char)
      MemberSubRoute.PointerSubRoute(
        memberName = member.name,
        baseAddress = baseAddress,
        memberOffsetBytes = memberOffset,
        isArray = member.isArray,
        arrayLength = member.arrayLength,
        wordSizeBits = wordSizeBits,
        endian = endian,
        pointeeType = Some(ptype),
        pointeeSizeBytes = IrJsonCodecs.pointeeSizeBytes(ptype, Some(wordSizeBits), errors),
        pointeeDecodeCodec = IrJsonCodecs.compileJsonCodecForType(
          ptype,
          endian,
          Some(wordSizeBits),
          errors,
          context
        ),
        followCString = isCharPointee
      )
    }
  }

  private[daphttp] def buildValueSubRoute(
      member: IrMember,
      memberOffset: Int,
      baseAddress: Long,
      endian: IrEndian,
      wordSizeBits: Int,
      errors: ListBuffer[String],
      context: String
  ): Option[MemberSubRoute.ValueSubRoute] = {
    val isFuncPointer = member.target.isInstanceOf[IrType.FunctionPointer]
    if (member.isPointer && !isFuncPointer) None
    else {
      val (valueType, elementSizeBytes) = member.target match {
        case fp: IrType.FunctionPointer =>
          (Some(fp), Some(wordSizeBits / 8))
        case listType: IrType.ListType =>
          val elemSize = IrJsonCodecs.pointeeSizeBytes(listType.element, Some(wordSizeBits), errors)
          (Some(listType.element), elemSize)
        case IrType.Primitive(kind) =>
          (
            Some(member.target),
            IrJsonCodecs.pointeeSizeBytes(member.target, Some(wordSizeBits), errors)
          )
        case intEnum: IrType.IntEnum =>
          (Some(intEnum), IrJsonCodecs.pointeeSizeBytes(intEnum, Some(wordSizeBits), errors))
        case struct: IrType.Struct =>
          (Some(struct), IrJsonCodecs.pointeeSizeBytes(struct, Some(wordSizeBits), errors))
        case _ =>
          (None, None)
      }
      Some(
        MemberSubRoute.ValueSubRoute(
          memberName = member.name,
          baseAddress = baseAddress,
          memberOffsetBytes = memberOffset,
          isArray = member.isArray,
          arrayLength = member.arrayLength,
          wordSizeBits = wordSizeBits,
          endian = endian,
          valueType = valueType,
          elementSizeBytes = elementSizeBytes,
          elementStrideBytes = IrJsonCodecs.arrayElementStrideBytes(member, Some(wordSizeBits)),
          decodeCodec = valueType.flatMap(vt =>
            IrJsonCodecs.compileJsonCodecForType(vt, endian, Some(wordSizeBits), errors, context)
          )
        )
      )
    }
  }

  private[daphttp] def collectReadsForType(
      irType: IrType,
      baseAddress: Option[Long],
      pathPrefix: String,
      endian: IrEndian,
      wordSize: Option[Int],
      errors: ListBuffer[String],
      sizeBytesOverride: Option[Int] = None
  ): List[ReadPlan] = {
    irType match {
      case struct: IrType.Struct =>
        val isDapShape = struct match {
          case _: IrType.Bitmask            => true
          case _: IrType.MemoryMappedStruct => true
          case _: IrType.EnclosingStruct    => false
        }
        if (isDapShape) {
          baseAddress match {
            case None =>
              errors += s"${struct.id}: DAP-backed structures must be reachable from @staticAddress members."
              Nil
            case Some(address) =>
              // Prefer the enclosing member's symbol/read @size when layout-inferred width differs
              // (e.g. after Smithy round-trip drops C offset comments).
              sizeBytesOverride.orElse(
                IrJsonCodecs.structureSizeBytes(struct, wordSize, errors)
              ) match {
                case Some(sizeBytes) =>
                  List(
                    ReadPlan(
                      path = pathPrefix,
                      address = address,
                      sizeBytes = sizeBytes,
                      decodeType = Some(struct),
                      endian = endian,
                      wordSizeBits = wordSize,
                      decodeCodec = IrJsonCodecs
                        .compileJsonCodec(Some(struct), endian, wordSize, errors, pathPrefix),
                      cStringPointer = false
                    )
                  )
                case None => Nil
              }
          }
        } else {
          struct.members.flatMap { member =>
            val memberPath = s"$pathPrefix.${member.name}"
            val memberRequiresStaticAddress = member.target match {
              case _: IrType.Bitmask            => true
              case _: IrType.MemoryMappedStruct => true
              case _                            => false
            }
            val memberAddress =
              if (memberRequiresStaticAddress) {
                member.staticAddress.orElse {
                  errors += s"${member.id}: DAP-backed members of non-DAP structures must declare @staticAddress."
                  None
                }
              } else {
                member.staticAddress
              }
            member.target match {
              case nestedStruct: IrType.Struct =>
                collectReadsForType(
                  nestedStruct,
                  memberAddress,
                  memberPath,
                  member.endianOverride.getOrElse(endian),
                  wordSize,
                  errors,
                  sizeBytesOverride = member.readSizeBytes
                )
              case _ =>
                memberAddress.flatMap { address =>
                  val memberEndian = member.endianOverride.getOrElse(endian)
                  val decodeCodec =
                    IrJsonCodecs.compileMemberCodec(
                      member,
                      memberEndian,
                      wordSize,
                      errors,
                      memberPath
                    )
                  IrJsonCodecs.memberSizeBytes(member, wordSize, errors).map { sizeBytes =>
                    val isCharPointer =
                      member.isPointer && IrJsonCodecs.memberReadType(member) == IrType.Primitive(
                        IrPrimitive.Char
                      )
                    ReadPlan(
                      path = memberPath,
                      address = address,
                      sizeBytes = sizeBytes,
                      decodeType = Some(IrJsonCodecs.memberReadType(member)),
                      endian = memberEndian,
                      wordSizeBits = wordSize,
                      decodeCodec = decodeCodec,
                      cStringPointer = isCharPointer && !member.isArray,
                      cStringPointerArray = isCharPointer && member.isArray,
                      elementStrideBytes = IrJsonCodecs.arrayElementStrideBytes(member, wordSize),
                      arrayLength = if (member.isArray) member.arrayLength else None
                    )
                  }
                }.toList
            }
          }
        }
      case union: IrType.Union =>
        errors += s"${union.id}: Union outputs are modeled in IR but not yet readable from static layouts."
        Nil
      case mapType: IrType.MapType =>
        errors += s"${mapType.id}: Map outputs are modeled in IR but not yet readable from static layouts."
        Nil
      case listType: IrType.ListType =>
        errors += s"${listType.id}: Top-level list outputs are modeled in IR but must be wrapped in a structure."
        Nil
      case _: IrType.Primitive =>
        errors += s"$pathPrefix: Primitive outputs are modeled in IR but must be wrapped in a structure."
        Nil
      case _: IrType.IntEnum =>
        errors += s"$pathPrefix: Enum outputs are modeled in IR but must be wrapped in a structure."
        Nil
      case ref: IrType.Ref =>
        errors += s"${ref.id}: Unsupported shape for route planning."
        Nil
      case _: IrType.FunctionPointer =>
        errors += s"$pathPrefix: Function pointer outputs must be wrapped in a structure."
        Nil
    }
  }

}
