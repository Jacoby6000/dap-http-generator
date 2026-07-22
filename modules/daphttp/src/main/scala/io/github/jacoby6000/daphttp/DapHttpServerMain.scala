package io.github.jacoby6000.daphttp

import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all._
import io.circe.Json
import io.circe.syntax._
import org.http4s.HttpRoutes
import org.http4s.Response
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.io._
import org.http4s.server.websocket.WebSocketBuilder2
import scodec.bits.BitVector
import software.amazon.smithy.model.Model

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.Base64
import scala.util.Try

/** HTTP/DAP runtime library (route planning, memory decode, DAP serving helpers).
  *
  * DESNOTE(jbarber, 2026-07-21): Process entry is [[Cli]] only. This object used to be a second
  * `IOApp` with a flat `--smithy=` arg parser that duplicated Ember/watch bootstrap; that path is
  * removed so lifecycle stays in one place. DAP client types live in [[DapClient]] /
  * [[SocketDapClient]] / [[LocalPipeDapClient]].
  */
object DapHttpServerMain {
  private[daphttp] def routes(
      plansRef: Ref[IO, RoutePlansLoadResult],
      dapClient: DapClient
  ): HttpRoutes[IO] =
    routes(
      plansRef,
      dapClient,
      Ref.unsafe[IO, OverlayEngine](OverlayEngine.empty),
      overlayPersistPath = None,
      watchService = None,
      wsBuilder = None
    )

  private[daphttp] def routes(
      plansRef: Ref[IO, RoutePlansLoadResult],
      dapClient: DapClient,
      overlaysRef: Ref[IO, OverlayEngine],
      overlayPersistPath: Option[Path]
  ): HttpRoutes[IO] =
    routes(plansRef, dapClient, overlaysRef, overlayPersistPath, None, None)

  private[daphttp] def routes(
      plansRef: Ref[IO, RoutePlansLoadResult],
      dapClient: DapClient,
      overlaysRef: Ref[IO, OverlayEngine],
      overlayPersistPath: Option[Path],
      watchService: Option[RealtimeWatchService],
      wsBuilder: Option[WebSocketBuilder2[IO]]
  ): HttpRoutes[IO] =
    DapProxyRoutes.routes(plansRef, dapClient, overlaysRef) <+>
      WebAppRoutes.routes(
        plansRef,
        overlaysRef,
        overlayPersistPath,
        watchService,
        wsBuilder
      ) <+>
      ApiRoutes.routes(plansRef, dapClient, overlaysRef)

  private[daphttp] def matchRoutePublic(
      path: String,
      routes: Map[String, RoutePlan]
  ): Option[(RoutePlan, List[Int])] =
    RoutePathResolver.matchRoute(path, routes)

  private[daphttp] def matchMemberSubRoutePublic(
      path: String,
      routes: Map[String, RoutePlan]
  ): Option[(RoutePlan, MemberSubRoute, Option[Int])] =
    RoutePathResolver.matchMemberSubRoute(path, routes)

  private[daphttp] def resolveMemberPathPublic(
      path: String,
      routes: Map[String, RoutePlan]
  ): Option[ResolvedMemberRead] =
    MemberPathResolver.resolve(path, routes)

  private def takeOverlayPrep(
      overlaysRef: Ref[IO, OverlayEngine],
      decodeType: Option[IrType],
      endian: IrEndian,
      wordSizeBits: Option[Int]
  ): IO[Option[OverlayEngine.PreparedCodec]] =
    MemoryDecodeService.prepareOverlay(overlaysRef, decodeType, endian, wordSizeBits)

  private[daphttp] def resolveAndAnnotateDecoded(
      valueType: Option[IrType],
      decoded: Json,
      address: Long,
      wordSizeBits: Int,
      endian: IrEndian,
      dapClient: DapClient
  ): IO[Json] =
    valueType match {
      case Some(struct: IrType.Struct) =>
        resolveStructPointers(
          struct,
          decoded,
          dapClient,
          Some(wordSizeBits),
          endian,
          Set.empty,
          0
        ).map { withPtrs =>
          HttpRouteIrEmitter.annotateDecodedAddresses(
            struct,
            withPtrs,
            address,
            Some(wordSizeBits)
          )
        }
      case Some(other) =>
        IO.pure(
          HttpRouteIrEmitter.annotateDecodedAddresses(
            other,
            decoded,
            address,
            Some(wordSizeBits)
          )
        )
      case None =>
        IO.pure(decoded)
    }

  private[daphttp] def decodeOverlayRegion(
      prep: OverlayEngine.PreparedCodec,
      base64Data: String,
      address: Long,
      wordSizeBits: Int,
      endian: IrEndian,
      dapClient: DapClient
  ): IO[Json] =
    decodeWithOverlayCodec(
      prep,
      base64Data,
      address,
      Some(wordSizeBits),
      endian,
      dapClient
    )

  private def readDecodedRegion(
      region: MemoryDecodeService.Region,
      dapClient: DapClient,
      overlaysRef: Ref[IO, OverlayEngine]
  ): IO[MemoryDecodeService.Outcome] =
    MemoryDecodeService.readAndDecode(
      region,
      dapClient,
      overlaysRef,
      resolveAndAnnotateDecoded,
      decodeOverlayRegion
    )

  private[daphttp] def serveRoutePlan(
      routePlan: RoutePlan,
      dapClient: DapClient,
      overlaysRef: Ref[IO, OverlayEngine]
  ): IO[Response[IO]] =
    routePlan.reads
      .foldLeft(IO.pure(List.empty[Json])) { (accIO, readPlan) =>
        for {
          acc <- accIO
          overlayPrep <- takeOverlayPrep(
            overlaysRef,
            readPlan.decodeType,
            readPlan.endian,
            readPlan.wordSizeBits
          )
          readSize =
            overlayPrep
              .map(o => math.max(readPlan.sizeBytes, o.sizeBytes))
              .getOrElse(readPlan.sizeBytes)
          read <- dapClient.readMemory(readPlan.address, readSize)
          decoded <- read match {
            case Right(data) =>
              val sourceData = truncateBase64ToBytes(data, readPlan.sizeBytes)
              decodeReadResult(readPlan, sourceData, dapClient)
            case Left(_) => IO.pure(Json.Null)
          }
          overlayDecoded <- (read, overlayPrep) match {
            case (Right(data), Some(prep)) =>
              decodeWithOverlayCodec(
                prep,
                data,
                readPlan.address,
                readPlan.wordSizeBits,
                readPlan.endian,
                dapClient
              ).map(Some(_))
            case _ =>
              IO.pure(None)
          }
        } yield {
          val readJson = read match {
            case Right(data) =>
              Json
                .obj(
                  "path" -> Json.fromString(readPlan.path),
                  "bytes" -> Json.fromInt(readSize),
                  "data" -> Json.fromString(data),
                  "decoded" -> decoded
                )
                .deepMerge(decodeTypeJson(readPlan.decodeType))
                .deepMerge(
                  overlayDecoded
                    .map(od => Json.obj("overlayDecoded" -> od))
                    .getOrElse(Json.obj())
                )
            case Left(error) =>
              Json.obj(
                "path" -> Json.fromString(readPlan.path),
                "bytes" -> Json.fromInt(readPlan.sizeBytes),
                "error" -> Json.fromString(error)
              )
          }
          acc :+ readJson
        }
      }
      .flatMap { reads =>
        Ok(
          Json.obj(
            "route" -> Json.fromString(routePlan.path),
            "reads" -> reads.asJson
          )
        )
      }

  private def decodeWithOverlayCodec(
      prep: OverlayEngine.PreparedCodec,
      base64Data: String,
      address: Long,
      wordSizeBits: Option[Int],
      endian: IrEndian,
      dapClient: DapClient
  ): IO[Json] = {
    val decoded = Try(Base64.getDecoder.decode(base64Data)).toOption
      .flatMap(bytes => prep.codec.decode(BitVector(bytes)).toOption.map(_.value))
      .getOrElse(Json.Null)
    resolveDecodedPointers(prep.irType, decoded, dapClient, wordSizeBits, endian, Set.empty, 0)
      .map { resolved =>
        HttpRouteIrEmitter.annotateDecodedAddresses(
          prep.irType,
          resolved,
          address,
          wordSizeBits
        )
      }
  }

  private def truncateBase64ToBytes(base64Data: String, sizeBytes: Int): String =
    MemoryDecodeService.truncateBase64ToBytes(base64Data, sizeBytes)

  private[daphttp] def servePointerChainRoute(
      routePlan: RoutePlan,
      chainSegments: List[Int],
      dapClient: DapClient,
      overlaysRef: Ref[IO, OverlayEngine]
  ): IO[Response[IO]] =
    routePlan.pointerChain match {
      case None =>
        NotFound(
          Json.obj("error" -> Json.fromString(s"No pointer chain route for ${routePlan.path}"))
        )
      case Some(chain) =>
        resolvePointerChainAddress(chain, chainSegments, dapClient).flatMap {
          case Left(error) =>
            BadRequest(Json.obj("error" -> Json.fromString(error)))
          case Right(rawAddress) =>
            val structAddress = maskToWordSize(rawAddress, Some(chain.wordSizeBits))
            if (shouldFollowCString(chain)) {
              readNullTerminatedCString(dapClient, structAddress).flatMap { value =>
                Ok(
                  Json.obj(
                    "route" -> Json.fromString(routePlan.path),
                    "segments" -> chainSegments.asJson,
                    "decoded" -> Json.fromString(value)
                  )
                )
              }
            } else {
              takeOverlayPrep(
                overlaysRef,
                Some(chain.pointeeType),
                chain.endian,
                Some(chain.wordSizeBits)
              ).flatMap { overlayPrep =>
                val readSize =
                  overlayPrep
                    .map(o => math.max(chain.pointeeSizeBytes, o.sizeBytes))
                    .getOrElse(chain.pointeeSizeBytes)
                dapClient.readMemory(structAddress, readSize).flatMap {
                  case Left(error) =>
                    Ok(
                      Json.obj(
                        "route" -> Json.fromString(routePlan.path),
                        "segments" -> chainSegments.asJson,
                        "error" -> Json.fromString(error)
                      )
                    )
                  case Right(data) =>
                    val sourceData = truncateBase64ToBytes(data, chain.pointeeSizeBytes)
                    val decoded = chain.pointeeDecodeCodec match {
                      case None        => Json.Null
                      case Some(codec) =>
                        Try(Base64.getDecoder.decode(sourceData)).toOption
                          .flatMap(bytes => codec.decode(BitVector(bytes)).toOption.map(_.value))
                          .getOrElse(Json.Null)
                    }
                    val resolvedDecoded = chain.pointeeType match {
                      case struct: IrType.Struct =>
                        resolveStructPointers(
                          struct,
                          decoded,
                          dapClient,
                          Some(chain.wordSizeBits),
                          chain.endian,
                          Set.empty,
                          0
                        ).map { resolved =>
                          HttpRouteIrEmitter.annotateDecodedAddresses(
                            struct,
                            resolved,
                            structAddress,
                            Some(chain.wordSizeBits)
                          )
                        }
                      case other =>
                        IO.pure(
                          HttpRouteIrEmitter.annotateDecodedAddresses(
                            other,
                            decoded,
                            structAddress,
                            Some(chain.wordSizeBits)
                          )
                        )
                    }
                    val overlayDecodedIO = overlayPrep match {
                      case Some(prep) =>
                        decodeWithOverlayCodec(
                          prep,
                          data,
                          structAddress,
                          Some(chain.wordSizeBits),
                          chain.endian,
                          dapClient
                        ).map(Some(_))
                      case None =>
                        IO.pure(None)
                    }
                    for {
                      finalDecoded <- resolvedDecoded
                      overlayDecoded <- overlayDecodedIO
                      response <- Ok(
                        Json
                          .obj(
                            "route" -> Json.fromString(routePlan.path),
                            "segments" -> chainSegments.asJson,
                            "bytes" -> Json.fromInt(readSize),
                            "data" -> Json.fromString(data),
                            "decoded" -> finalDecoded
                          )
                          .deepMerge(
                            overlayDecoded
                              .map(od => Json.obj("overlayDecoded" -> od))
                              .getOrElse(Json.obj())
                          )
                      )
                    } yield response
                }
              }
            }
        }
    }

  private[daphttp] def serveResolvedMember(
      routePath: String,
      resolved: ResolvedMemberRead,
      dapClient: DapClient,
      overlaysRef: Ref[IO, OverlayEngine]
  ): IO[Response[IO]] =
    readDecodedRegion(
      MemoryDecodeService.Region(
        address = resolved.address,
        sourceSizeBytes = resolved.sizeBytes,
        endian = resolved.endian,
        wordSizeBits = resolved.wordSizeBits,
        valueType = resolved.valueType,
        decodeCodec = resolved.decodeCodec
      ),
      dapClient,
      overlaysRef
    ).flatMap {
      case MemoryDecodeService.Outcome.Failed(error) =>
        Ok(
          Json.obj(
            "route" -> Json.fromString(routePath),
            "error" -> Json.fromString(error)
          )
        )
      case MemoryDecodeService.Outcome.Success(readSize, _, finalDecoded, overlayDecoded) =>
        Ok(
          Json
            .obj(
              "route" -> Json.fromString(routePath),
              "bytes" -> Json.fromInt(readSize),
              "decoded" -> finalDecoded
            )
            .deepMerge(decodeTypeJson(resolved.valueType))
            .deepMerge(
              overlayDecoded
                .map(od => Json.obj("overlayDecoded" -> od))
                .getOrElse(Json.obj())
            )
        )
    }

  private[daphttp] def serveMemberSubRoute(
      routePath: String,
      sub: MemberSubRoute,
      index: Option[Int],
      dapClient: DapClient,
      overlaysRef: Ref[IO, OverlayEngine]
  ): IO[Response[IO]] =
    sub match {
      case v: MemberSubRoute.ValueSubRoute =>
        serveValueSubRoute(routePath, v, index, dapClient, overlaysRef)
      case p: MemberSubRoute.PointerSubRoute =>
        servePointerSubRoute(routePath, p, index, dapClient, overlaysRef)
    }

  private def serveValueSubRoute(
      routePath: String,
      sub: MemberSubRoute.ValueSubRoute,
      index: Option[Int],
      dapClient: DapClient,
      overlaysRef: Ref[IO, OverlayEngine]
  ): IO[Response[IO]] = {
    val elementSize = sub.elementSizeBytes.getOrElse(0)
    val elementStride = sub.elementStrideBytes.getOrElse(elementSize)
    val readAddress = sub.baseAddress + sub.memberOffsetBytes.toLong +
      index.map(_.toLong * elementStride).getOrElse(0L)
    readDecodedRegion(
      MemoryDecodeService.Region(
        address = readAddress,
        sourceSizeBytes = elementSize,
        endian = sub.endian,
        wordSizeBits = sub.wordSizeBits,
        valueType = sub.valueType,
        decodeCodec = sub.decodeCodec
      ),
      dapClient,
      overlaysRef
    ).flatMap {
      case MemoryDecodeService.Outcome.Failed(error) =>
        Ok(
          Json.obj(
            "route" -> Json.fromString(routePath),
            "member" -> Json.fromString(sub.memberName),
            "index" -> index.map(Json.fromInt).getOrElse(Json.Null),
            "error" -> Json.fromString(error)
          )
        )
      case MemoryDecodeService.Outcome.Success(readSize, _, finalDecoded, overlayDecoded) =>
        Ok(
          Json
            .obj(
              "route" -> Json.fromString(routePath),
              "member" -> Json.fromString(sub.memberName),
              "index" -> index.map(Json.fromInt).getOrElse(Json.Null),
              "bytes" -> Json.fromInt(readSize),
              "decoded" -> finalDecoded
            )
            .deepMerge(decodeTypeJson(sub.valueType))
            .deepMerge(
              overlayDecoded
                .map(od => Json.obj("overlayDecoded" -> od))
                .getOrElse(Json.obj())
            )
        )
    }
  }

  private def servePointerSubRoute(
      routePath: String,
      sub: MemberSubRoute.PointerSubRoute,
      index: Option[Int],
      dapClient: DapClient,
      overlaysRef: Ref[IO, OverlayEngine]
  ): IO[Response[IO]] = {
    val wordBytes = sub.wordSizeBits / 8
    val pointerAddress = sub.baseAddress + sub.memberOffsetBytes.toLong + index
      .map(_.toLong * wordBytes)
      .getOrElse(0L)

    def readPointerAt(address: Long): IO[Either[String, Long]] =
      dapClient.readMemory(address, wordBytes).map {
        case Left(error) => Left(error)
        case Right(data) =>
          Try(Base64.getDecoder.decode(data)).toOption match {
            case Some(bytes) =>
              Right(PointerChainResolver.pointerValue(bytes, sub.endian))
            case None =>
              Left("Failed to decode pointer bytes from DAP response.")
          }
      }

    readPointerAt(pointerAddress).flatMap {
      case Left(error) =>
        Ok(
          Json.obj(
            "route" -> Json.fromString(routePath),
            "pointerAddress" -> Json.fromString(DapAddress.format(pointerAddress)),
            "error" -> Json.fromString(error)
          )
        )
      case Right(targetAddress) =>
        val masked = maskToWordSize(targetAddress, Some(sub.wordSizeBits))
        if (sub.followCString) {
          readNullTerminatedCString(dapClient, masked).flatMap { value =>
            Ok(
              Json.obj(
                "route" -> Json.fromString(routePath),
                "member" -> Json.fromString(sub.memberName),
                "index" -> index.map(Json.fromInt).getOrElse(Json.Null),
                "pointerAddress" -> Json.fromString(DapAddress.format(pointerAddress)),
                "decoded" -> Json.fromString(value)
              )
            )
          }
        } else {
          sub.pointeeSizeBytes match {
            case None =>
              Ok(
                Json.obj(
                  "route" -> Json.fromString(routePath),
                  "member" -> Json.fromString(sub.memberName),
                  "index" -> index.map(Json.fromInt).getOrElse(Json.Null),
                  "pointerAddress" -> Json.fromString(DapAddress.format(pointerAddress)),
                  "error" -> Json.fromString("Unable to determine pointee size.")
                )
              )
            case Some(sizeBytes) =>
              readDecodedRegion(
                MemoryDecodeService.Region(
                  address = masked,
                  sourceSizeBytes = sizeBytes,
                  endian = sub.endian,
                  wordSizeBits = sub.wordSizeBits,
                  valueType = sub.pointeeType,
                  decodeCodec = sub.pointeeDecodeCodec
                ),
                dapClient,
                overlaysRef
              ).flatMap {
                case MemoryDecodeService.Outcome.Failed(error) =>
                  Ok(
                    Json.obj(
                      "route" -> Json.fromString(routePath),
                      "member" -> Json.fromString(sub.memberName),
                      "index" -> index.map(Json.fromInt).getOrElse(Json.Null),
                      "pointerAddress" -> Json.fromString(DapAddress.format(pointerAddress)),
                      "error" -> Json.fromString(error)
                    )
                  )
                case MemoryDecodeService.Outcome
                      .Success(readSize, _, finalDecoded, overlayDecoded) =>
                  Ok(
                    Json
                      .obj(
                        "route" -> Json.fromString(routePath),
                        "member" -> Json.fromString(sub.memberName),
                        "index" -> index.map(Json.fromInt).getOrElse(Json.Null),
                        "pointerAddress" -> Json.fromString(DapAddress.format(pointerAddress)),
                        "bytes" -> Json.fromInt(readSize),
                        "decoded" -> finalDecoded
                      )
                      .deepMerge(decodeTypeJson(sub.pointeeType))
                      .deepMerge(
                        overlayDecoded
                          .map(od => Json.obj("overlayDecoded" -> od))
                          .getOrElse(Json.obj())
                      )
                  )
              }
          }
        }
    }
  }

  private def resolvePointerChainAddress(
      chain: PointerChainPlan,
      segments: List[Int],
      dapClient: DapClient
  ): IO[Either[String, Long]] = {
    val wordBytes = chain.wordSizeBits / 8

    def readPointer(address: Long): IO[Either[String, Long]] =
      dapClient.readMemory(address, wordBytes).map {
        case Left(error) => Left(error)
        case Right(data) =>
          Try(Base64.getDecoder.decode(data)).toOption match {
            case Some(bytes) => Right(PointerChainResolver.pointerValue(bytes, chain.endian))
            case None        => Left("Failed to decode pointer bytes from DAP response.")
          }
      }

    if (segments.length != PointerChainResolver.requiredSegmentCount(chain)) {
      IO.pure(
        Left(
          s"Expected ${PointerChainResolver.requiredSegmentCount(chain)} index segment(s), got ${segments.length}."
        )
      )
    } else if (chain.outerArrayLength.isDefined) {
      val outerIndex = segments.head
      val innerSegments = segments.tail
      val outerStrideBytes = chain.outerElementStrideBytes.getOrElse(wordBytes)
      val outerAddress = chain.baseAddress + outerIndex.toLong * outerStrideBytes
      readPointer(outerAddress).flatMap {
        case Left(error)    => IO.pure(Left(error))
        case Right(pointer) =>
          resolveInnerPointerChain(pointer, innerSegments, wordBytes, readPointer)
      }
    } else {
      resolveInnerPointerChain(chain.baseAddress, segments, wordBytes, readPointer)
    }
  }

  private def resolveInnerPointerChain(
      pointer: Long,
      segments: List[Int],
      wordBytes: Int,
      readPointer: Long => IO[Either[String, Long]]
  ): IO[Either[String, Long]] =
    segments.foldLeft(IO.pure[Either[String, Long]](Right(pointer))) { case (accIO, index) =>
      accIO.flatMap {
        case Left(error)    => IO.pure(Left(error))
        case Right(current) =>
          readPointer(current + index.toLong * wordBytes)
      }
    }

  def buildRoutePlansFromModel(model: Model): RoutePlansLoadResult =
    SmithyIrGenerator.generateFromModel(model) match {
      case Left(errors) =>
        RoutePlansLoadResult(Map.empty, errors)
      case Right(services) =>
        compileServicesWithSizingWarnings(services)
    }

  private def compileServicesWithSizingWarnings(
      services: List[IrService]
  ): RoutePlansLoadResult = {
    IrSizingWarnings.writeToStderr(services)
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(services)
    plans.copy(services = services)
  }

  private def decodeReadResult(
      readPlan: ReadPlan,
      base64Data: String,
      dapClient: DapClient
  ): IO[Json] =
    if (readPlan.cStringPointer) {
      decodeCStringPointer(readPlan, base64Data, dapClient)
    } else if (readPlan.cStringPointerArray) {
      decodeCStringPointerArray(readPlan, base64Data, dapClient)
    } else {
      val decoded = readPlan.decodeCodec match {
        case None        => Json.Null
        case Some(codec) =>
          Try(Base64.getDecoder.decode(base64Data)).toOption
            .flatMap(bytes => codec.decode(BitVector(bytes)).toOption.map(_.value))
            .getOrElse(Json.Null)
      }
      readPlan.decodeType match {
        case Some(irType) =>
          resolveDecodedPointers(
            irType,
            decoded,
            dapClient,
            readPlan.wordSizeBits,
            readPlan.endian,
            Set.empty,
            0
          ).map { resolved =>
            HttpRouteIrEmitter.annotateDecodedAddresses(
              irType,
              resolved,
              readPlan.address,
              readPlan.wordSizeBits,
              readPlan.elementStrideBytes
            )
          }
        case None =>
          IO.pure(decoded)
      }
    }

  private def resolveDecodedPointers(
      irType: IrType,
      decoded: Json,
      dapClient: DapClient,
      wordSizeBits: Option[Int],
      endian: IrEndian,
      visited: Set[Long],
      depth: Int
  ): IO[Json] =
    irType match {
      case struct: IrType.Struct =>
        resolveStructPointers(struct, decoded, dapClient, wordSizeBits, endian, visited, depth)
      case listType: IrType.ListType =>
        listType.element match {
          case struct: IrType.Struct =>
            decoded.asArray match {
              case Some(elements) =>
                elements
                  .foldLeft(IO.pure(Vector.empty[Json])) { (accIO, element) =>
                    accIO.flatMap { acc =>
                      resolveStructPointers(
                        struct,
                        element,
                        dapClient,
                        wordSizeBits,
                        endian,
                        visited,
                        depth
                      )
                        .map(acc :+ _)
                    }
                  }
                  .map(resolved => Json.arr(resolved: _*))
              case None =>
                IO.pure(decoded)
            }
          case _ =>
            IO.pure(decoded)
        }
      case _ =>
        IO.pure(decoded)
    }

  /** Follows char* as C strings and non-function pointer members by reading pointees into the
    * parent decode (so pointer arrays become arrays of decoded values in the UI).
    */
  private def resolveStructPointers(
      struct: IrType.Struct,
      decoded: Json,
      dapClient: DapClient,
      wordSizeBits: Option[Int],
      endian: IrEndian,
      visited: Set[Long],
      depth: Int
  ): IO[Json] =
    struct.members.foldLeft(IO.pure(decoded)) { (accIO, member) =>
      accIO.flatMap { json =>
        val memberEndian = member.endianOverride.getOrElse(endian)
        val isCharPointer =
          member.isPointer && member.primitiveOverride.contains(IrPrimitive.Char)
        val isFunctionPointer = member.target.isInstanceOf[IrType.FunctionPointer]
        if (isCharPointer && member.isArray) {
          json.hcursor.downField(member.name).as[List[Json]] match {
            case Right(elements) =>
              elements
                .foldLeft(IO.pure(Vector.empty[Json])) { (acc2, element) =>
                  acc2.flatMap { vec =>
                    element.as[Long].toOption match {
                      case Some(rawAddr) =>
                        val addr = maskToWordSize(rawAddr, wordSizeBits)
                        readNullTerminatedCString(dapClient, addr)
                          .map(value => vec :+ Json.fromString(value))
                      case None =>
                        IO.pure(vec :+ element)
                    }
                  }
                }
                .map { strs =>
                  json.mapObject(_.add(member.name, Json.arr(strs: _*)))
                }
            case _ =>
              IO.pure(json)
          }
        } else if (isCharPointer && !member.isArray) {
          json.hcursor.downField(member.name).as[Long].toOption match {
            case Some(rawAddr) =>
              val addr = maskToWordSize(rawAddr, wordSizeBits)
              readNullTerminatedCString(dapClient, addr).map { value =>
                json.mapObject(_.add(member.name, Json.fromString(value)))
              }
            case None =>
              IO.pure(json)
          }
        } else if (member.isPointer && !isFunctionPointer) {
          pointeeTypeForMember(member) match {
            case None =>
              IO.pure(json)
            case Some(pointeeType) =>
              if (member.isArray) {
                json.hcursor.downField(member.name).as[List[Json]] match {
                  case Right(elements) =>
                    elements
                      .foldLeft(IO.pure(Vector.empty[Json])) { (acc2, element) =>
                        acc2.flatMap { vec =>
                          element.as[Long].toOption match {
                            case Some(rawAddr) =>
                              dereferencePointer(
                                rawAddr,
                                pointeeType,
                                dapClient,
                                wordSizeBits,
                                memberEndian,
                                visited,
                                depth
                              ).map(vec :+ _)
                            case None =>
                              IO.pure(vec :+ element)
                          }
                        }
                      }
                      .map { resolved =>
                        json.mapObject(_.add(member.name, Json.arr(resolved: _*)))
                      }
                  case _ =>
                    IO.pure(json)
                }
              } else {
                json.hcursor.downField(member.name).as[Long].toOption match {
                  case Some(rawAddr) =>
                    dereferencePointer(
                      rawAddr,
                      pointeeType,
                      dapClient,
                      wordSizeBits,
                      memberEndian,
                      visited,
                      depth
                    ).map { resolved =>
                      json.mapObject(_.add(member.name, resolved))
                    }
                  case None =>
                    IO.pure(json)
                }
              }
          }
        } else {
          member.target match {
            case nested: IrType.Struct if !member.isPointer =>
              json.hcursor.downField(member.name).focus match {
                case Some(nestedJson) =>
                  resolveStructPointers(
                    nested,
                    nestedJson,
                    dapClient,
                    wordSizeBits,
                    memberEndian,
                    visited,
                    depth
                  )
                    .map { resolved =>
                      json.mapObject(_.add(member.name, resolved))
                    }
                case None =>
                  IO.pure(json)
              }
            case _ =>
              IO.pure(json)
          }
        }
      }
    }

  private def pointeeTypeForMember(member: IrMember): Option[IrType] =
    member.target match {
      case listType: IrType.ListType if member.isArray =>
        listType.element match {
          case _: IrType.FunctionPointer => None
          case element                   => Some(element)
        }
      case _: IrType.FunctionPointer =>
        None
      case other if member.isPointer && !member.isArray =>
        Some(other)
      case _ =>
        None
    }

  // DESNOTE(jbarber, 2026-07-21): Melee graphs can form pointer cycles (e.g. self / back
  // links). Bound follow depth and visited addresses so decode cannot hang or blow the stack.
  private val MaxPointerFollowDepth = 8

  private def dereferencePointer(
      rawAddr: Long,
      pointeeType: IrType,
      dapClient: DapClient,
      wordSizeBits: Option[Int],
      endian: IrEndian,
      visited: Set[Long],
      depth: Int
  ): IO[Json] = {
    val addr = maskToWordSize(rawAddr, wordSizeBits)
    if (addr == 0L) {
      IO.pure(Json.Null)
    } else if (depth >= MaxPointerFollowDepth || visited.contains(addr)) {
      IO.pure(Json.fromLong(addr))
    } else {
      val nextVisited = visited + addr
      (
        HttpRouteIrEmitter.sizeBytesForType(pointeeType, wordSizeBits),
        HttpRouteIrEmitter.compileCodec(pointeeType, endian, wordSizeBits)
      ) match {
        case (Right(size), Right(codec)) =>
          dapClient.readMemory(addr, size).flatMap {
            case Left(_) =>
              // Distinguish unread/unreachable pointees from NULL (addr == 0 → Json.Null above).
              IO.pure(Json.fromLong(addr))
            case Right(data) =>
              val decoded = Try(Base64.getDecoder.decode(data)).toOption
                .flatMap(bytes => codec.decode(BitVector(bytes)).toOption.map(_.value))
                .getOrElse(Json.Null)
              resolveDecodedPointers(
                pointeeType,
                decoded,
                dapClient,
                wordSizeBits,
                endian,
                nextVisited,
                depth + 1
              ).map { resolved =>
                markPointerPointee(
                  HttpRouteIrEmitter.annotateDecodedAddresses(
                    pointeeType,
                    resolved,
                    addr,
                    wordSizeBits
                  )
                )
              }
          }
        case _ =>
          IO.pure(Json.fromLong(addr))
      }
    }
  }

  private def markPointerPointee(json: Json): Json =
    json.asObject match {
      case Some(obj) => Json.fromJsonObject(obj.add("_pointer", Json.True))
      case None      => json
    }

  private def decodeTypeJson(irType: Option[IrType]): Json =
    irType
      .flatMap(decodeTypeId)
      .map(id => Json.obj("decodeType" -> Json.fromString(id)))
      .getOrElse(Json.obj())

  private def decodeTypeId(irType: IrType): Option[String] =
    irType match {
      case s: IrType.Struct          => Some(s.id.toString)
      case list: IrType.ListType     => Some(list.id.toString)
      case intEnum: IrType.IntEnum   => Some(intEnum.id.toString)
      case union: IrType.Union       => Some(union.id.toString)
      case mapType: IrType.MapType   => Some(mapType.id.toString)
      case IrType.Ref(id)            => Some(id.toString)
      case _: IrType.Primitive       => None
      case _: IrType.FunctionPointer => None
    }

  // DESNOTE(jbarber, 2026-07-19): Prefer an unsigned right-shift mask over `(1L << bits) - 1`.
  // JVM `long` shifts already keep the low 6 bits of the count (so `1L << 32` is fine), but
  // reviewers commonly confuse that with `int` shifts where `1 << 32` collapses to `1`.
  // `-1L >>> (64 - bits)` makes the 32-bit mask (`0xffffffffL`) obvious without that pitfall.
  // See https://docs.oracle.com/javase/specs/jls/se21/html/jls-15.html#jls-15.19
  private[daphttp] def maskToWordSize(value: Long, wordSizeBits: Option[Int]): Long =
    wordSizeBits match {
      case Some(bits) if bits > 0 && bits < 64 => value & (-1L >>> (64 - bits))
      case _                                   => value
    }

  private def decodeCStringPointerArray(
      readPlan: ReadPlan,
      base64Data: String,
      dapClient: DapClient
  ): IO[Json] = {
    val decoded = readPlan.decodeCodec match {
      case None        => Json.Null
      case Some(codec) =>
        Try(Base64.getDecoder.decode(base64Data)).toOption
          .flatMap(bytes => codec.decode(BitVector(bytes)).toOption.map(_.value))
          .getOrElse(Json.Null)
    }
    decoded.asArray match {
      case Some(elements) =>
        elements
          .foldLeft(IO.pure(Vector.empty[Json])) { (acc, element) =>
            acc.flatMap { vec =>
              element.as[Long].toOption match {
                case Some(rawAddr) =>
                  val addr = maskToWordSize(rawAddr, readPlan.wordSizeBits)
                  readNullTerminatedCString(dapClient, addr)
                    .map(value => vec :+ Json.fromString(value))
                case None =>
                  IO.pure(vec :+ element)
              }
            }
          }
          .map(Json.arr)
      case None =>
        IO.pure(decoded)
    }
  }

  private def shouldFollowCString(chain: PointerChainPlan): Boolean =
    chain.followCString || chain.pointeeType == IrType.Primitive(IrPrimitive.Char)

  private def readNullTerminatedCString(
      dapClient: DapClient,
      address: Long,
      chunkSize: Int = 256,
      maxBytes: Int = 4096
  ): IO[String] = {
    def readChunk(currentAddress: Long, acc: Vector[Byte]): IO[Vector[Byte]] =
      if (acc.size >= maxBytes) {
        IO.pure(acc)
      } else {
        val requestSize = math.min(chunkSize, maxBytes - acc.size)
        dapClient.readMemory(currentAddress, requestSize).flatMap {
          case Left(_) =>
            IO.pure(acc)
          case Right(data) =>
            Try(Base64.getDecoder.decode(data)).toOption match {
              case None =>
                IO.pure(acc)
              case Some(bytes) if bytes.isEmpty =>
                IO.pure(acc)
              case Some(bytes) =>
                val nullIndex = bytes.indexWhere(_ == 0)
                if (nullIndex >= 0) {
                  IO.pure(acc ++ bytes.take(nullIndex))
                } else {
                  readChunk(currentAddress + bytes.length, acc ++ bytes)
                }
            }
        }
      }

    readChunk(address, Vector.empty).map(bytes =>
      new String(bytes.toArray, StandardCharsets.US_ASCII)
    )
  }

  private def pointerValue(bytes: Array[Byte], endian: IrEndian): Long =
    PointerChainResolver.pointerValue(bytes, endian)

  private def decodeCStringPointer(
      readPlan: ReadPlan,
      base64Data: String,
      dapClient: DapClient
  ): IO[Json] = {
    val wordBytes = readPlan.wordSizeBits.map(_ / 8).getOrElse(4)
    val pointer = Try(Base64.getDecoder.decode(base64Data)).toOption.map { bytes =>
      pointerValue(bytes.take(wordBytes), readPlan.endian)
    }
    pointer match {
      case None          => IO.pure(Json.Null)
      case Some(address) =>
        val masked = maskToWordSize(address, readPlan.wordSizeBits)
        readNullTerminatedCString(dapClient, masked).map(Json.fromString)
    }
  }
}
