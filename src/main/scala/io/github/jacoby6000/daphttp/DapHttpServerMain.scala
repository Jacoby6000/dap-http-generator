package io.github.jacoby6000.daphttp

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Ref
import cats.syntax.all._
import com.comcast.ip4s.Host
import com.comcast.ip4s.Port
import io.circe.Json
import io.circe.syntax._
import org.http4s.HttpRoutes
import org.http4s.Response
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.websocket.WebSocketBuilder2
import scodec.bits.BitVector
import software.amazon.smithy.model.Model

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.net.InetSocketAddress
import java.net.Socket
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.Base64
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

object DapHttpServerMain extends IOApp {
  private final case class Config(
      smithyPaths: List[Path],
      dapPipe: Option[Path],
      dapHost: String,
      dapPort: Int,
      dapTimeoutMs: Int,
      dapContinueTimeoutMs: Int,
      dapConnectTimeoutMs: Int,
      dapConnectRetryMs: Int,
      bindHost: String,
      bindPort: Int,
      watch: Boolean
  )

  override def run(args: List[String]): IO[ExitCode] = {
    parseArgs(args) match {
      case Left(error) =>
        IO.println(error).as(ExitCode.Error)
      case Right(config) =>
        for {
          plansRef <- Ref.of[IO, RoutePlansLoadResult](loadPlans(config.smithyPaths))
          dapClient = DapClients.create(
            config.dapPipe,
            config.dapHost,
            config.dapPort,
            config.dapTimeoutMs,
            config.dapContinueTimeoutMs,
            config.dapConnectTimeoutMs,
            config.dapConnectRetryMs
          )
          overlaysRef <- Ref.of[IO, OverlayEngine](OverlayEngine.empty)
          watchService <- RealtimeWatchService.create(dapClient, plansRef, overlaysRef)
          _ <- watchSmithySources(config, plansRef, overlaysRef, watchService)
          _ <- dapClient.startConnectionManager()
          _ <- watchService.start()
          exit <- EmberServerBuilder
            .default[IO]
            .withHost(Host.fromString(config.bindHost).getOrElse(Host.fromString("0.0.0.0").get))
            .withPort(Port.fromInt(config.bindPort).getOrElse(Port.fromInt(8080).get))
            // DESNOTE(jbarber, 2026-07-20): Default Ember idle is 60s, which drops quiet /ws
            // sockets. Server Ping frames keep them alive; this is a safety margin for brief gaps.
            .withIdleTimeout(5.minutes)
            .withHttpWebSocketApp { wsBuilder =>
              HttpLoggingMiddleware(
                routes(plansRef, dapClient, overlaysRef, None, watchService, wsBuilder).orNotFound
              )
            }
            .build
            .use(_ => IO.never)
            .as(ExitCode.Success)
        } yield exit
    }
  }

  private def parseArgs(args: List[String]): Either[String, Config] = {
    val values = args.flatMap { arg =>
      arg.split("=", 2).toList match {
        case key :: value :: Nil if key.startsWith("--") => Some(key.drop(2) -> value)
        case _                                           => None
      }
    }.toMap

    val smithyPaths = values
      .get("smithy")
      .map(_.split(",").toList.filter(_.nonEmpty).map(Paths.get(_)))
      .getOrElse(Nil)

    if (smithyPaths.isEmpty) {
      Left("Missing required --smithy=/path/a,/path/b argument.")
    } else {
      Right(
        Config(
          smithyPaths = smithyPaths,
          dapPipe = values.get("dapPipe").map(Paths.get(_)),
          dapHost = values.getOrElse("dapHost", "127.0.0.1"),
          dapPort = values.get("dapPort").flatMap(v => Try(v.toInt).toOption).getOrElse(4711),
          dapTimeoutMs =
            values.get("dapTimeoutMs").flatMap(v => Try(v.toInt).toOption).getOrElse(5000),
          dapContinueTimeoutMs = values
            .get("dapContinueTimeoutMs")
            .flatMap(v => Try(v.toInt).toOption)
            .getOrElse(30000),
          dapConnectTimeoutMs =
            values.get("dapConnectTimeoutMs").flatMap(v => Try(v.toInt).toOption).getOrElse(1000),
          dapConnectRetryMs =
            values.get("dapConnectRetryMs").flatMap(v => Try(v.toInt).toOption).getOrElse(5000),
          bindHost = values.getOrElse("bindHost", "0.0.0.0"),
          bindPort = values.get("bindPort").flatMap(v => Try(v.toInt).toOption).getOrElse(8080),
          watch = values.get("watch").forall(_.toBooleanOption.getOrElse(true))
        )
      )
    }
  }

  private[daphttp] def routes(
      plansRef: Ref[IO, RoutePlansLoadResult],
      dapClient: DapClient
  ): HttpRoutes[IO] =
    routes(
      plansRef,
      dapClient,
      Ref.unsafe[IO, OverlayEngine](OverlayEngine.empty),
      overlayPersistPath = None,
      watchService = null,
      wsBuilder = null
    )

  private[daphttp] def routes(
      plansRef: Ref[IO, RoutePlansLoadResult],
      dapClient: DapClient,
      overlaysRef: Ref[IO, OverlayEngine],
      overlayPersistPath: Option[Path]
  ): HttpRoutes[IO] =
    routes(plansRef, dapClient, overlaysRef, overlayPersistPath, null, null)

  private[daphttp] def routes(
      plansRef: Ref[IO, RoutePlansLoadResult],
      dapClient: DapClient,
      overlaysRef: Ref[IO, OverlayEngine],
      overlayPersistPath: Option[Path],
      watchService: RealtimeWatchService,
      wsBuilder: WebSocketBuilder2[IO]
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
    matchRoute(path, routes)

  private[daphttp] def matchMemberSubRoutePublic(
      path: String,
      routes: Map[String, RoutePlan]
  ): Option[(RoutePlan, MemberSubRoute, Option[Int])] =
    matchMemberSubRoute(path, routes)

  private[daphttp] def resolveMemberPathPublic(
      path: String,
      routes: Map[String, RoutePlan]
  ): Option[ResolvedMemberRead] =
    MemberPathResolver.resolve(path, routes)

  private def matchRoute(
      path: String,
      routes: Map[String, RoutePlan]
  ): Option[(RoutePlan, List[Int])] =
    routes.get(path).map(_ -> Nil).orElse {
      routes.collectFirst {
        case (basePath, plan)
            if plan.pointerChain.isDefined && path.startsWith(
              s"$basePath/"
            ) && path.length > basePath.length =>
          val suffix = path.stripPrefix(s"$basePath/")
          if (
            suffix.nonEmpty && suffix
              .split("/")
              .forall(segment => segment.nonEmpty && segment.forall(_.isDigit))
          ) {
            Some(plan -> suffix.split("/").map(_.toInt).toList)
          } else {
            None
          }
      }.flatten
    }

  private def matchMemberSubRoute(
      path: String,
      routes: Map[String, RoutePlan]
  ): Option[(RoutePlan, MemberSubRoute, Option[Int])] =
    routes.collectFirst(Function.unlift { case (basePath, plan: RoutePlan) =>
      if (!path.startsWith(s"$basePath/") || path.length <= basePath.length + 1) None
      else {
        val suffix = path.stripPrefix(s"$basePath/")
        val parts = suffix.split("/").toList
        matchRootArrayElement(plan, parts).orElse {
          parts.headOption.flatMap { memberName =>
            plan.memberSubRoutes
              .find(s => s.memberName == memberName && s.memberName.nonEmpty)
              .flatMap { sub =>
                parts.drop(1) match {
                  case Nil if !sub.isArray =>
                    Some((plan, sub, None))
                  case Nil if sub.isArray =>
                    None
                  case indexStr :: Nil if sub.isArray && indexStr.forall(_.isDigit) =>
                    Some((plan, sub, Some(indexStr.toInt)))
                  case _ =>
                    None
                }
              }
          }
        }
      }
    })

  private def matchRootArrayElement(
      plan: RoutePlan,
      parts: List[String]
  ): Option[(RoutePlan, MemberSubRoute, Option[Int])] =
    plan.memberSubRoutes
      .find(s => s.memberName == MemberSubRoute.RootArrayMemberName && s.isArray)
      .flatMap { sub =>
        parts match {
          case indexStr :: Nil if indexStr.forall(_.isDigit) =>
            Some((plan, sub, Some(indexStr.toInt)))
          case _ =>
            None
        }
      }

  private def takeOverlayPrep(
      overlaysRef: Ref[IO, OverlayEngine],
      decodeType: Option[IrType],
      endian: IrEndian,
      wordSizeBits: Option[Int]
  ): IO[Option[OverlayEngine.PreparedCodec]] =
    decodeType match {
      case None =>
        IO.pure(None)
      case Some(irType) =>
        overlaysRef.modify { engine =>
          engine.prepare(irType, endian, wordSizeBits)
        }
    }

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
    Try(Base64.getDecoder.decode(base64Data)).toOption
      .map { bytes =>
        val truncated =
          if (bytes.length <= sizeBytes) bytes
          else java.util.Arrays.copyOf(bytes, sizeBytes)
        Base64.getEncoder.encodeToString(truncated)
      }
      .getOrElse(base64Data)

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
          case Right(structAddress) =>
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
  ): IO[Response[IO]] = {
    val sizeBytes = resolved.sizeBytes
    if (sizeBytes <= 0) {
      Ok(
        Json.obj(
          "route" -> Json.fromString(routePath),
          "error" -> Json.fromString("Unable to determine member size.")
        )
      )
    } else {
      takeOverlayPrep(
        overlaysRef,
        resolved.valueType,
        resolved.endian,
        Some(resolved.wordSizeBits)
      ).flatMap { overlayPrep =>
        val readSize =
          overlayPrep.map(o => math.max(sizeBytes, o.sizeBytes)).getOrElse(sizeBytes)
        dapClient.readMemory(resolved.address, readSize).flatMap {
          case Left(error) =>
            Ok(
              Json.obj(
                "route" -> Json.fromString(routePath),
                "error" -> Json.fromString(error)
              )
            )
          case Right(data) =>
            val sourceData = truncateBase64ToBytes(data, sizeBytes)
            val decoded = resolved.decodeCodec match {
              case None        => Json.Null
              case Some(codec) =>
                Try(Base64.getDecoder.decode(sourceData)).toOption
                  .flatMap(bytes => codec.decode(BitVector(bytes)).toOption.map(_.value))
                  .getOrElse(Json.Null)
            }
            val resolvedDecoded = resolved.valueType match {
              case Some(struct: IrType.Struct) =>
                resolveStructPointers(
                  struct,
                  decoded,
                  dapClient,
                  Some(resolved.wordSizeBits),
                  resolved.endian,
                  Set.empty,
                  0
                ).map { withPtrs =>
                  HttpRouteIrEmitter.annotateDecodedAddresses(
                    struct,
                    withPtrs,
                    resolved.address,
                    Some(resolved.wordSizeBits)
                  )
                }
              case Some(other) =>
                IO.pure(
                  HttpRouteIrEmitter.annotateDecodedAddresses(
                    other,
                    decoded,
                    resolved.address,
                    Some(resolved.wordSizeBits)
                  )
                )
              case None =>
                IO.pure(decoded)
            }
            val overlayDecodedIO = overlayPrep match {
              case Some(prep) =>
                decodeWithOverlayCodec(
                  prep,
                  data,
                  resolved.address,
                  Some(resolved.wordSizeBits),
                  resolved.endian,
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
            } yield response
        }
      }
    }
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
    sub.elementSizeBytes match {
      case None =>
        Ok(
          Json.obj(
            "route" -> Json.fromString(routePath),
            "member" -> Json.fromString(sub.memberName),
            "index" -> index.map(Json.fromInt).getOrElse(Json.Null),
            "error" -> Json.fromString("Unable to determine member size.")
          )
        )
      case Some(sizeBytes) =>
        takeOverlayPrep(
          overlaysRef,
          sub.valueType,
          sub.endian,
          Some(sub.wordSizeBits)
        ).flatMap { overlayPrep =>
          val readSize =
            overlayPrep.map(o => math.max(sizeBytes, o.sizeBytes)).getOrElse(sizeBytes)
          dapClient.readMemory(readAddress, readSize).flatMap {
            case Left(error) =>
              Ok(
                Json.obj(
                  "route" -> Json.fromString(routePath),
                  "member" -> Json.fromString(sub.memberName),
                  "index" -> index.map(Json.fromInt).getOrElse(Json.Null),
                  "error" -> Json.fromString(error)
                )
              )
            case Right(data) =>
              val sourceData = truncateBase64ToBytes(data, sizeBytes)
              val decoded = sub.decodeCodec match {
                case None        => Json.Null
                case Some(codec) =>
                  Try(Base64.getDecoder.decode(sourceData)).toOption
                    .flatMap(bytes => codec.decode(BitVector(bytes)).toOption.map(_.value))
                    .getOrElse(Json.Null)
              }
              val resolvedDecoded = sub.valueType match {
                case Some(struct: IrType.Struct) =>
                  resolveStructPointers(
                    struct,
                    decoded,
                    dapClient,
                    Some(sub.wordSizeBits),
                    sub.endian,
                    Set.empty,
                    0
                  ).map { resolved =>
                    HttpRouteIrEmitter.annotateDecodedAddresses(
                      struct,
                      resolved,
                      readAddress,
                      Some(sub.wordSizeBits)
                    )
                  }
                case Some(other) =>
                  IO.pure(
                    HttpRouteIrEmitter.annotateDecodedAddresses(
                      other,
                      decoded,
                      readAddress,
                      Some(sub.wordSizeBits)
                    )
                  )
                case None =>
                  IO.pure(decoded)
              }
              val overlayDecodedIO = overlayPrep match {
                case Some(prep) =>
                  decodeWithOverlayCodec(
                    prep,
                    data,
                    readAddress,
                    Some(sub.wordSizeBits),
                    sub.endian,
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
              } yield response
          }
        }
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
            "pointerAddress" -> Json.fromString(f"0x$pointerAddress%x"),
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
                "pointerAddress" -> Json.fromString(f"0x$pointerAddress%x"),
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
                  "pointerAddress" -> Json.fromString(f"0x$pointerAddress%x"),
                  "error" -> Json.fromString("Unable to determine pointee size.")
                )
              )
            case Some(sizeBytes) =>
              takeOverlayPrep(
                overlaysRef,
                sub.pointeeType,
                sub.endian,
                Some(sub.wordSizeBits)
              ).flatMap { overlayPrep =>
                val readSize =
                  overlayPrep.map(o => math.max(sizeBytes, o.sizeBytes)).getOrElse(sizeBytes)
                dapClient.readMemory(masked, readSize).flatMap {
                  case Left(error) =>
                    Ok(
                      Json.obj(
                        "route" -> Json.fromString(routePath),
                        "member" -> Json.fromString(sub.memberName),
                        "index" -> index.map(Json.fromInt).getOrElse(Json.Null),
                        "pointerAddress" -> Json.fromString(f"0x$pointerAddress%x"),
                        "error" -> Json.fromString(error)
                      )
                    )
                  case Right(data) =>
                    val sourceData = truncateBase64ToBytes(data, sizeBytes)
                    val decoded = sub.pointeeDecodeCodec match {
                      case None        => Json.Null
                      case Some(codec) =>
                        Try(Base64.getDecoder.decode(sourceData)).toOption
                          .flatMap(bytes => codec.decode(BitVector(bytes)).toOption.map(_.value))
                          .getOrElse(Json.Null)
                    }
                    val resolvedDecoded = sub.pointeeType match {
                      case Some(struct: IrType.Struct) =>
                        resolveStructPointers(
                          struct,
                          decoded,
                          dapClient,
                          Some(sub.wordSizeBits),
                          sub.endian,
                          Set.empty,
                          0
                        ).map { resolved =>
                          HttpRouteIrEmitter.annotateDecodedAddresses(
                            struct,
                            resolved,
                            masked,
                            Some(sub.wordSizeBits)
                          )
                        }
                      case Some(other) =>
                        IO.pure(
                          HttpRouteIrEmitter.annotateDecodedAddresses(
                            other,
                            decoded,
                            masked,
                            Some(sub.wordSizeBits)
                          )
                        )
                      case None =>
                        IO.pure(decoded)
                    }
                    val overlayDecodedIO = overlayPrep match {
                      case Some(prep) =>
                        decodeWithOverlayCodec(
                          prep,
                          data,
                          masked,
                          Some(sub.wordSizeBits),
                          sub.endian,
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
                            "route" -> Json.fromString(routePath),
                            "member" -> Json.fromString(sub.memberName),
                            "index" -> index.map(Json.fromInt).getOrElse(Json.Null),
                            "pointerAddress" -> Json.fromString(f"0x$pointerAddress%x"),
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
                    } yield response
                }
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

  private def loadPlans(smithyPaths: List[Path]): RoutePlansLoadResult =
    loadModel(smithyPaths) match {
      case Left(errors) =>
        RoutePlansLoadResult(Map.empty, errors)
      case Right(model) =>
        buildRoutePlansFromModel(model)
    }

  private def loadModel(smithyPaths: List[Path]): Either[List[String], Model] = {
    val smithyFiles = smithyPaths.flatMap(collectSmithyFiles).distinct
    val traitsPath = SmithyIrEmitter.dapHttpTraitsPath
    val assembler = Model.assembler()
    if (Files.exists(traitsPath)) {
      assembler.addImport(traitsPath.toString)
    }
    smithyFiles.foreach(path => assembler.addImport(path.toString))
    val result = assembler.assemble()
    if (result.isBroken) {
      Left(result.getValidationEvents.asScala.map(_.toString).toList)
    } else {
      Right(result.unwrap())
    }
  }

  private def collectSmithyFiles(path: Path): List[Path] = {
    if (!Files.exists(path)) {
      Nil
    } else if (Files.isRegularFile(path) && path.toString.endsWith(".smithy")) {
      List(path)
    } else if (Files.isDirectory(path)) {
      val stream = Files.walk(path)
      try {
        stream
          .iterator()
          .asScala
          .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".smithy"))
          .toList
      } finally {
        stream.close()
      }
    } else {
      Nil
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
          IO.pure(Json.fromLong(rawAddr))
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
        readNullTerminatedCString(dapClient, address).map(Json.fromString)
    }
  }

  private def watchSmithySources(
      config: Config,
      plansRef: Ref[IO, RoutePlansLoadResult],
      overlaysRef: Ref[IO, OverlayEngine],
      watchService: RealtimeWatchService
  ): IO[Unit] = {
    if (!config.watch) {
      IO.unit
    } else {
      def newestTimestamp(paths: List[Path]): Long =
        paths
          .flatMap(collectSmithyFiles)
          .flatMap(path => Try(Files.getLastModifiedTime(path).toMillis).toOption)
          .sorted
          .lastOption
          .getOrElse(0L)

      def loop(lastSeen: Long): IO[Unit] =
        IO.sleep(2.seconds) *> IO.blocking(newestTimestamp(config.smithyPaths)).flatMap { newest =>
          if (newest > lastSeen) {
            for {
              plans <- IO.blocking(loadPlans(config.smithyPaths))
              _ <- plansRef.set(plans)
              engine <- overlaysRef.get
              _ <- overlaysRef.set(OverlayEngine.fromServices(engine.document, plans.services))
              _ <- watchService.rebindAll
              _ <- loop(newest)
            } yield ()
          } else {
            loop(lastSeen)
          }
        }
      IO.blocking(newestTimestamp(config.smithyPaths)).flatMap(ts => loop(ts).start.void)
    }
  }

  private[daphttp] trait DapClient {
    def readMemory(address: Long, sizeBytes: Int): IO[Either[String, String]]
    def writeMemory(address: Long, dataBase64: String): IO[Either[String, Int]] = {
      val _ = (address, dataBase64)
      IO.pure(Left("writeMemory is not supported by this DAP client."))
    }
    def continueExecution(threadId: Option[Int] = None): IO[Either[String, Json]]
    def startConnectionManager(): IO[Unit] = IO.unit
    def realtimeWatch(address: Long, count: Int): IO[Either[String, WatchHandle]] = {
      val _ = (address, count)
      IO.pure(Left("Realtime watch is not supported by this DAP client."))
    }
    def realtimeWatchCancel(watchId: Int): IO[Either[String, Unit]] = {
      val _ = watchId
      IO.pure(Left("Realtime watch cancel is not supported by this DAP client."))
    }
    def memoryChanged: fs2.Stream[IO, MemoryChangedEvent] = fs2.Stream.empty
    def sessionResets: fs2.Stream[IO, Unit] = fs2.Stream.empty
  }

  private[daphttp] final class SocketDapClient(
      host: String,
      port: Int,
      dapTimeoutMs: Int = 5000,
      dapContinueTimeoutMs: Int = 30000,
      dapConnectTimeoutMs: Int = 1000,
      dapConnectRetryMs: Int = 5000
  ) extends DapClient {
    private val connectionLock = new AnyRef
    private var session: Option[DapFramedSession] = None
    private val memoryChangedTopic = DapEventBus.createMemoryChangedTopic()
    private val sessionResetTopic = DapEventBus.createSessionResetTopic()

    private[daphttp] def isConnected: Boolean =
      connectionLock.synchronized(session.exists(_.isOpen))

    override def memoryChanged: fs2.Stream[IO, MemoryChangedEvent] =
      memoryChangedTopic.subscribe(256)

    override def sessionResets: fs2.Stream[IO, Unit] =
      sessionResetTopic.subscribe(32)

    override def startConnectionManager(): IO[Unit] = {
      def maintainConnection: IO[Unit] =
        IO.blocking(isConnected).flatMap {
          case true =>
            IO.sleep(dapConnectRetryMs.millis) *> maintainConnection
          case false =>
            IO.blocking(tryEstablishSession()) flatMap {
              case Right(_) =>
                DapHttpLoggers.dap.info(
                  "DAP session ready host={} port={}",
                  host,
                  Integer.valueOf(port)
                )
                IO.sleep(dapConnectRetryMs.millis) *> maintainConnection
              case Left(error) =>
                DapHttpLoggers.dap.warn(
                  "DAP connect failed ({}); retrying in {} ms",
                  error,
                  Integer.valueOf(dapConnectRetryMs)
                )
                IO.sleep(dapConnectRetryMs.millis) *> maintainConnection
            }
        }

      maintainConnection.start.void
    }

    override def readMemory(address: Long, sizeBytes: Int): IO[Either[String, String]] =
      IO.blocking {
        DapHttpLoggers.dap.debug(
          "readMemory host={} port={} address=0x{} bytes={}",
          host,
          Integer.valueOf(port),
          java.lang.Long.toHexString(address),
          Integer.valueOf(sizeBytes)
        )

        withPersistentSession(dapTimeoutMs) { activeSession =>
          activeSession
            .sendRequest(
              command = "readMemory",
              arguments = Some(
                Json.obj(
                  "memoryReference" -> Json.fromString(f"0x$address%x"),
                  "count" -> Json.fromInt(sizeBytes)
                )
              ),
              timeoutMs = dapTimeoutMs
            )
            .flatMap { body =>
              body.hcursor
                .downField("data")
                .as[String]
                .toOption
                .toRight("DAP readMemory response did not include body.data.")
            } match {
            case Right(value) =>
              DapHttpLoggers.dap.debug(
                "readMemory address=0x{} succeeded bytes={}",
                java.lang.Long.toHexString(address),
                Integer.valueOf(sizeBytes)
              )
              Right(value)
            case Left(error) =>
              DapHttpLoggers.dap.warn(
                "readMemory address=0x{} failed: {}",
                java.lang.Long.toHexString(address),
                error
              )
              Left(error)
          }
        }
      }.handleError { error =>
        DapHttpLoggers.dap.warn(
          "readMemory address=0x{} failed: {}",
          java.lang.Long.toHexString(address),
          error.getMessage
        )
        Left(error.getMessage)
      }

    override def writeMemory(address: Long, dataBase64: String): IO[Either[String, Int]] =
      IO.blocking {
        DapHttpLoggers.dap.debug(
          "writeMemory host={} port={} address=0x{}",
          host,
          Integer.valueOf(port),
          java.lang.Long.toHexString(address)
        )
        withPersistentSession(dapTimeoutMs) { activeSession =>
          activeSession
            .sendRequest(
              command = "writeMemory",
              arguments = Some(
                Json.obj(
                  "memoryReference" -> Json.fromString(f"0x$address%x"),
                  "data" -> Json.fromString(dataBase64)
                )
              ),
              timeoutMs = dapTimeoutMs
            )
            .flatMap { body =>
              body.hcursor
                .get[Int]("bytesWritten")
                .toOption
                .toRight("DAP writeMemory response did not include body.bytesWritten.")
            } match {
            case Right(written) =>
              DapHttpLoggers.dap.debug(
                "writeMemory address=0x{} succeeded bytesWritten={}",
                java.lang.Long.toHexString(address),
                Integer.valueOf(written)
              )
              Right(written)
            case Left(error) =>
              DapHttpLoggers.dap.warn(
                "writeMemory address=0x{} failed: {}",
                java.lang.Long.toHexString(address),
                error
              )
              Left(error)
          }
        }
      }.handleError { error =>
        DapHttpLoggers.dap.warn(
          "writeMemory address=0x{} failed: {}",
          java.lang.Long.toHexString(address),
          error.getMessage
        )
        Left(error.getMessage)
      }

    override def realtimeWatch(address: Long, count: Int): IO[Either[String, WatchHandle]] =
      IO.blocking {
        withPersistentSession(dapTimeoutMs) { activeSession =>
          activeSession.realtimeWatch(address, count, dapTimeoutMs)
        }
      }.handleError(error => Left(error.getMessage))

    override def realtimeWatchCancel(watchId: Int): IO[Either[String, Unit]] =
      IO.blocking {
        withPersistentSession(dapTimeoutMs) { activeSession =>
          activeSession.realtimeWatchCancel(watchId, dapTimeoutMs)
        }
      }.handleError(error => Left(error.getMessage))

    override def continueExecution(threadId: Option[Int] = None): IO[Either[String, Json]] =
      IO.blocking {
        DapHttpLoggers.dap.info("continue host={} port={}", host, Integer.valueOf(port))
        withPersistentSession(dapContinueTimeoutMs) { activeSession =>
          val resolvedThreadId = threadId.getOrElse {
            activeSession
              .sendRequest(
                command = "threads",
                arguments = None,
                timeoutMs = math.min(dapTimeoutMs, 2000)
              )
              .toOption
              .flatMap(json => parseThreadIds(json).headOption)
              .getOrElse {
                DapHttpLoggers.dap.debug("threads unavailable; continuing with threadId=1")
                1
              }
          }

          activeSession
            .sendRequest(
              command = "continue",
              arguments = Some(Json.obj("threadId" -> Json.fromInt(resolvedThreadId))),
              timeoutMs = dapContinueTimeoutMs
            )
            .map { response =>
              DapHttpLoggers.dap.info(
                "continue threadId={} succeeded",
                Integer.valueOf(resolvedThreadId)
              )
              response
            }
            .left
            .map { error =>
              DapHttpLoggers.dap.warn(
                "continue threadId={} failed: {}",
                Integer.valueOf(resolvedThreadId),
                error
              )
              error
            }
        }
      }.handleError { error =>
        DapHttpLoggers.dap.warn("continue failed: {}", error.getMessage)
        Left(error.getMessage)
      }

    private def withPersistentSession[A](timeoutMs: Int)(
        f: DapFramedSession => A
    ): A = {
      def run(retrying: Boolean): A =
        connectionLock.synchronized {
          val activeSession = ensureSession(timeoutMs)
          try {
            f(activeSession)
          } catch {
            case error: Exception =>
              DapHttpLoggers.dap.warn(
                "DAP connection error (retrying={}): {}",
                java.lang.Boolean.valueOf(!retrying),
                error.getMessage
              )
              invalidateSession()
              if (!retrying) run(retrying = true)
              else throw error
          }
        }

      run(retrying = false)
    }

    private def ensureSession(timeoutMs: Int): DapFramedSession =
      connectionLock.synchronized {
        session match {
          case Some(activeSession) if activeSession.isOpen =>
            activeSession
          case stale =>
            if (stale.isDefined) invalidateSessionUnlocked()
            establishSession(timeoutMs, dapConnectTimeoutMs) match {
              case Right(activeSession) =>
                session = Some(activeSession)
                activeSession
              case Left(error) =>
                throw new java.io.IOException(error)
            }
        }
      }

    private def tryEstablishSession(): Either[String, Unit] =
      connectionLock.synchronized {
        session match {
          case Some(activeSession) if activeSession.isOpen =>
            Right(())
          case stale =>
            if (stale.isDefined) invalidateSessionUnlocked()
            establishSession(dapTimeoutMs, dapConnectTimeoutMs).map { activeSession =>
              session = Some(activeSession)
            }
        }
      }

    private def establishSession(
        requestTimeoutMs: Int,
        connectTimeoutMs: Int
    ): Either[String, DapFramedSession] = {
      DapHttpLoggers.dap.info(
        "connecting DAP session host={} port={}",
        host,
        Integer.valueOf(port)
      )
      val socket = new Socket()
      try {
        socket.connect(new InetSocketAddress(host, port), connectTimeoutMs)
        // Short SO timeout so the reader thread can wake periodically; request waits use Futures.
        socket.setSoTimeout(math.min(requestTimeoutMs, 1000))
        val activeSession = new DapFramedSession(
          new BufferedInputStream(socket.getInputStream),
          new BufferedOutputStream(socket.getOutputStream),
          event => DapEventBus.publish(memoryChangedTopic, event),
          () =>
            try socket.close()
            catch { case _: Exception => () }
        )
        // First request runs the DAP handshake; threads may fail on some adapters after init.
        activeSession.sendRequest("threads", None, requestTimeoutMs) match {
          case Left(error) if !activeSession.isOpen =>
            activeSession.close()
            Left(error)
          case Left(_) | Right(_) =>
            Right(activeSession)
        }
      } catch {
        case error: Exception =>
          try {
            socket.close()
          } catch {
            case _: Exception => ()
          }
          Left(error.getMessage)
      }
    }

    private def invalidateSession(): Unit =
      connectionLock.synchronized {
        invalidateSessionUnlocked()
      }

    private def invalidateSessionUnlocked(): Unit = {
      session.foreach(_.close())
      session = None
      DapEventBus.publish(sessionResetTopic, ())
    }

    private def parseThreadIds(responseBody: Json): List[Int] =
      responseBody.hcursor
        .downField("threads")
        .values
        .getOrElse(Vector.empty)
        .flatMap(_.hcursor.downField("id").as[Int].toOption)
        .toList
  }
}
