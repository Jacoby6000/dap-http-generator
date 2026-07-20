package io.github.jacoby6000.daphttp

import cats.data.OptionT
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Ref
import com.comcast.ip4s.Host
import com.comcast.ip4s.Port
import io.circe.Json
import io.circe.syntax._
import org.http4s.HttpRoutes
import org.http4s.MediaType
import org.http4s.Method.GET
import org.http4s.Method.POST
import org.http4s.Request
import org.http4s.Response
import org.http4s.StaticFile
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.`Content-Type`
import org.http4s.implicits._
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
          _ <- watchSmithySources(config, plansRef)
          dapClient = DapClients.create(
            config.dapPipe,
            config.dapHost,
            config.dapPort,
            config.dapTimeoutMs,
            config.dapContinueTimeoutMs,
            config.dapConnectTimeoutMs,
            config.dapConnectRetryMs
          )
          _ <- dapClient.startConnectionManager()
          app = HttpLoggingMiddleware(
            DapHttpServerMain.routes(plansRef, dapClient).orNotFound
          )
          exit <- EmberServerBuilder
            .default[IO]
            .withHost(Host.fromString(config.bindHost).getOrElse(Host.fromString("0.0.0.0").get))
            .withPort(Port.fromInt(config.bindPort).getOrElse(Port.fromInt(8080).get))
            .withHttpApp(app)
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
    HttpRoutes.of[IO] {
      case GET -> Root / "health" =>
        Ok(Json.obj("status" -> Json.fromString("ok")))

      case GET -> Root / "routes" =>
        plansRef.get.flatMap { result =>
          Ok(
            Json.obj(
              "routes" -> RouteTree.flatPaths(result.routes).asJson,
              "tree" -> RouteTree.fromPlans(result.routes).asJson,
              "errors" -> result.errors.asJson
            )
          )
        }

      case POST -> Root / "resume" =>
        dapClient.continueExecution().flatMap {
          case Right(response) =>
            Ok(Json.obj("status" -> Json.fromString("ok"), "dap" -> response))
          case Left(error) =>
            InternalServerError(Json.obj("error" -> Json.fromString(error)))
        }

      case request @ GET -> Root =>
        serveWebAsset(request, "index.html")

      case request @ GET -> Root / "assets" / fileName =>
        serveWebAsset(request, fileName)

      case request @ GET -> _ =>
        val routePath = request.uri.path.renderString
        if (!ApiRoutes.isDataPath(routePath)) {
          NotFound(Json.obj("error" -> Json.fromString(s"No route generated for $routePath")))
        } else {
          plansRef.get.flatMap { result =>
            matchRoute(routePath, result.routes) match {
              case Some((routePlan, chainSegments)) if chainSegments.nonEmpty =>
                servePointerChainRoute(routePlan, chainSegments, dapClient)
              case Some((routePlan, _)) =>
                serveRoutePlan(routePlan, dapClient)
              case None =>
                matchMemberSubRoute(routePath, result.routes) match {
                  case Some((_, subRoute, index)) =>
                    serveMemberSubRoute(routePath, subRoute, index, dapClient)
                  case None =>
                    NotFound(
                      Json.obj("error" -> Json.fromString(s"No route generated for $routePath"))
                    )
                }
            }
          }
        }
    }

  private def serveWebAsset(request: Request[IO], fileName: String): IO[Response[IO]] = {
    val safeName = Paths.get(fileName).getFileName.toString
    val resourcePath = s"/web/$safeName"
    StaticFile
      .fromResource[IO](resourcePath, Some(request))
      .orElse {
        // Fallback when the Scala.js bundle has not been packaged yet (e.g. tests).
        if (safeName == "index.html")
          OptionT.liftF(Ok(fallbackIndexHtml, `Content-Type`(MediaType.text.html)))
        else OptionT.none[IO, Response[IO]]
      }
      .getOrElseF(NotFound())
  }

  private val fallbackIndexHtml: String =
    """<!DOCTYPE html>
      |<html lang="en">
      |<head>
      |  <meta charset="utf-8"/>
      |  <title>dap-http</title>
      |  <style>
      |    body { font-family: ui-monospace, monospace; margin: 2rem; background: #0f1419; color: #e7ecf1; }
      |    a { color: #7eb8ff; }
      |  </style>
      |</head>
      |<body>
      |  <h1>dap-http</h1>
      |  <p>UI assets are not packaged. Run <code>sbt compile</code> to build the Scala.js bundle, or use the JSON API:</p>
      |  <ul>
      |    <li><a href="/routes">/routes</a></li>
      |    <li><a href="/health">/health</a></li>
      |  </ul>
      |</body>
      |</html>
      |""".stripMargin

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
        val parts = suffix.split("/")
        parts.headOption.flatMap { memberName =>
          plan.memberSubRoutes.find(_.memberName == memberName).flatMap { sub =>
            parts.toList.drop(1) match {
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
    })

  private def serveRoutePlan(routePlan: RoutePlan, dapClient: DapClient): IO[Response[IO]] =
    routePlan.reads
      .foldLeft(IO.pure(List.empty[Json])) { (accIO, readPlan) =>
        for {
          acc <- accIO
          read <- dapClient.readMemory(readPlan.address, readPlan.sizeBytes)
          decoded <- read match {
            case Right(data) => decodeReadResult(readPlan, data, dapClient)
            case Left(_)     => IO.pure(Json.Null)
          }
        } yield {
          val readJson = read match {
            case Right(data) =>
              Json.obj(
                "path" -> Json.fromString(readPlan.path),
                "bytes" -> Json.fromInt(readPlan.sizeBytes),
                "data" -> Json.fromString(data),
                "decoded" -> decoded
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

  private def servePointerChainRoute(
      routePlan: RoutePlan,
      chainSegments: List[Int],
      dapClient: DapClient
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
              dapClient.readMemory(structAddress, chain.pointeeSizeBytes).flatMap {
                case Left(error) =>
                  Ok(
                    Json.obj(
                      "route" -> Json.fromString(routePlan.path),
                      "segments" -> chainSegments.asJson,
                      "error" -> Json.fromString(error)
                    )
                  )
                case Right(data) =>
                  val decoded = chain.pointeeDecodeCodec match {
                    case None        => Json.Null
                    case Some(codec) =>
                      Try(Base64.getDecoder.decode(data)).toOption
                        .flatMap(bytes => codec.decode(BitVector(bytes)).toOption.map(_.value))
                        .getOrElse(Json.Null)
                  }
                  val resolvedDecoded = chain.pointeeType match {
                    case struct: IrType.Struct =>
                      resolveStructCStringPointers(
                        struct,
                        decoded,
                        dapClient,
                        Some(chain.wordSizeBits)
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
                  resolvedDecoded.flatMap { finalDecoded =>
                    Ok(
                      Json.obj(
                        "route" -> Json.fromString(routePlan.path),
                        "segments" -> chainSegments.asJson,
                        "bytes" -> Json.fromInt(chain.pointeeSizeBytes),
                        "data" -> Json.fromString(data),
                        "decoded" -> finalDecoded
                      )
                    )
                  }
              }
            }
        }
    }

  private def serveMemberSubRoute(
      routePath: String,
      sub: MemberSubRoute,
      index: Option[Int],
      dapClient: DapClient
  ): IO[Response[IO]] =
    sub match {
      case v: MemberSubRoute.ValueSubRoute =>
        serveValueSubRoute(routePath, v, index, dapClient)
      case p: MemberSubRoute.PointerSubRoute =>
        servePointerSubRoute(routePath, p, index, dapClient)
    }

  private def serveValueSubRoute(
      routePath: String,
      sub: MemberSubRoute.ValueSubRoute,
      index: Option[Int],
      dapClient: DapClient
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
        dapClient.readMemory(readAddress, sizeBytes).flatMap {
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
            val decoded = sub.decodeCodec match {
              case None        => Json.Null
              case Some(codec) =>
                Try(Base64.getDecoder.decode(data)).toOption
                  .flatMap(bytes => codec.decode(BitVector(bytes)).toOption.map(_.value))
                  .getOrElse(Json.Null)
            }
            val resolvedDecoded = sub.valueType match {
              case Some(struct: IrType.Struct) =>
                resolveStructCStringPointers(
                  struct,
                  decoded,
                  dapClient,
                  Some(sub.wordSizeBits)
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
            resolvedDecoded.flatMap { finalDecoded =>
              Ok(
                Json.obj(
                  "route" -> Json.fromString(routePath),
                  "member" -> Json.fromString(sub.memberName),
                  "index" -> index.map(Json.fromInt).getOrElse(Json.Null),
                  "bytes" -> Json.fromInt(sizeBytes),
                  "decoded" -> finalDecoded
                )
              )
            }
        }
    }
  }

  private def servePointerSubRoute(
      routePath: String,
      sub: MemberSubRoute.PointerSubRoute,
      index: Option[Int],
      dapClient: DapClient
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
              dapClient.readMemory(masked, sizeBytes).flatMap {
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
                  val decoded = sub.pointeeDecodeCodec match {
                    case None        => Json.Null
                    case Some(codec) =>
                      Try(Base64.getDecoder.decode(data)).toOption
                        .flatMap(bytes => codec.decode(BitVector(bytes)).toOption.map(_.value))
                        .getOrElse(Json.Null)
                  }
                  val resolvedDecoded = sub.pointeeType match {
                    case Some(struct: IrType.Struct) =>
                      resolveStructCStringPointers(
                        struct,
                        decoded,
                        dapClient,
                        Some(sub.wordSizeBits)
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
                  resolvedDecoded.flatMap { finalDecoded =>
                    Ok(
                      Json.obj(
                        "route" -> Json.fromString(routePath),
                        "member" -> Json.fromString(sub.memberName),
                        "index" -> index.map(Json.fromInt).getOrElse(Json.Null),
                        "pointerAddress" -> Json.fromString(f"0x$pointerAddress%x"),
                        "bytes" -> Json.fromInt(sizeBytes),
                        "decoded" -> finalDecoded
                      )
                    )
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
    HttpRouteIrEmitter.emitRoutePlansFromIr(services)
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
          resolveDecodedCStringPointers(irType, decoded, dapClient, readPlan.wordSizeBits).map {
            resolved =>
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

  private def resolveDecodedCStringPointers(
      irType: IrType,
      decoded: Json,
      dapClient: DapClient,
      wordSizeBits: Option[Int]
  ): IO[Json] =
    irType match {
      case struct: IrType.Struct =>
        resolveStructCStringPointers(struct, decoded, dapClient, wordSizeBits)
      case listType: IrType.ListType =>
        listType.element match {
          case struct: IrType.Struct =>
            decoded.asArray match {
              case Some(elements) =>
                elements
                  .foldLeft(IO.pure(Vector.empty[Json])) { (accIO, element) =>
                    accIO.flatMap { acc =>
                      resolveStructCStringPointers(struct, element, dapClient, wordSizeBits)
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

  private def resolveStructCStringPointers(
      struct: IrType.Struct,
      decoded: Json,
      dapClient: DapClient,
      wordSizeBits: Option[Int]
  ): IO[Json] =
    struct.members.foldLeft(IO.pure(decoded)) { (accIO, member) =>
      accIO.flatMap { json =>
        val isCharPointerArray =
          member.isPointer && member.isArray && member.primitiveOverride.contains(IrPrimitive.Char)
        val isSingleCharPointer =
          member.isPointer && !member.isArray && member.primitiveOverride.contains(IrPrimitive.Char)
        if (isCharPointerArray) {
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
        } else if (isSingleCharPointer) {
          json.hcursor.downField(member.name).as[Long].toOption match {
            case Some(rawAddr) =>
              val addr = maskToWordSize(rawAddr, wordSizeBits)
              readNullTerminatedCString(dapClient, addr).map { value =>
                json.mapObject(_.add(member.name, Json.fromString(value)))
              }
            case None =>
              IO.pure(json)
          }
        } else {
          member.target match {
            case nested: IrType.Struct =>
              json.hcursor.downField(member.name).focus match {
                case Some(nestedJson) =>
                  resolveStructCStringPointers(nested, nestedJson, dapClient, wordSizeBits).map {
                    resolved =>
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
      plansRef: Ref[IO, RoutePlansLoadResult]
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
            plansRef.set(loadPlans(config.smithyPaths)) *> loop(newest)
          } else {
            loop(lastSeen)
          }
        }
      IO.blocking(newestTimestamp(config.smithyPaths)).flatMap(ts => loop(ts).start.void)
    }
  }

  private[daphttp] trait DapClient {
    def readMemory(address: Long, sizeBytes: Int): IO[Either[String, String]]
    def continueExecution(): IO[Either[String, Json]]
    def startConnectionManager(): IO[Unit] = IO.unit
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
    private var session: Option[DapSocketSession] = None

    private[daphttp] def isConnected: Boolean =
      connectionLock.synchronized(session.exists(_.isOpen))

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
              )
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

    override def continueExecution(): IO[Either[String, Json]] =
      IO.blocking {
        DapHttpLoggers.dap.info("continue host={} port={}", host, Integer.valueOf(port))
        withPersistentSession(dapContinueTimeoutMs) { activeSession =>
          val threadId =
            activeSession
              .trySendRequest(
                command = "threads",
                arguments = None,
                requestTimeoutMs = math.min(dapTimeoutMs, 2000)
              )
              .flatMap(json => parseThreadIds(json).headOption)
              .getOrElse {
                DapHttpLoggers.dap.debug("threads unavailable; continuing with threadId=1")
                1
              }

          activeSession
            .sendRequest(
              command = "continue",
              arguments = Some(Json.obj("threadId" -> Json.fromInt(threadId)))
            )
            .map { response =>
              DapHttpLoggers.dap.info("continue threadId={} succeeded", Integer.valueOf(threadId))
              response
            }
            .left
            .map { error =>
              DapHttpLoggers.dap.warn(
                "continue threadId={} failed: {}",
                Integer.valueOf(threadId),
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
        f: DapSocketSession => A
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

    private def ensureSession(timeoutMs: Int): DapSocketSession =
      connectionLock.synchronized {
        session.filter(_.isOpen) match {
          case Some(activeSession) =>
            activeSession.setTimeout(timeoutMs)
            activeSession
          case None =>
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
        session.filter(_.isOpen) match {
          case Some(_) => Right(())
          case None    =>
            establishSession(dapTimeoutMs, dapConnectTimeoutMs).map { activeSession =>
              session = Some(activeSession)
            }
        }
      }

    private def establishSession(
        requestTimeoutMs: Int,
        connectTimeoutMs: Int
    ): Either[String, DapSocketSession] = {
      DapHttpLoggers.dap.info(
        "connecting DAP session host={} port={}",
        host,
        Integer.valueOf(port)
      )
      val socket = new Socket()
      try {
        socket.connect(new InetSocketAddress(host, port), connectTimeoutMs)
        socket.setSoTimeout(requestTimeoutMs)
        val activeSession = new DapSocketSession(
          socket,
          new BufferedOutputStream(socket.getOutputStream),
          new BufferedInputStream(socket.getInputStream),
          requestTimeoutMs
        )
        activeSession.initialize().map(_ => activeSession)
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
        session.foreach(_.close())
        session = None
      }

    private final class DapSocketSession(
        socket: Socket,
        out: BufferedOutputStream,
        in: BufferedInputStream,
        initialTimeoutMs: Int
    ) {
      private var seqCounter = 1
      private var initialized = false
      private var timeoutMs = initialTimeoutMs

      def isOpen: Boolean = !socket.isClosed && socket.isConnected

      def setTimeout(ms: Int): Unit = {
        timeoutMs = ms
        socket.setSoTimeout(ms)
      }

      def close(): Unit =
        try {
          socket.close()
        } catch {
          case _: Exception => ()
        }

      def sendRequest(command: String, arguments: Option[Json]): Either[String, Json] =
        initialize().flatMap { _ =>
          val requestSeq = nextSeq()
          writeRequest(requestSeq, command, arguments)
          readUntilResponse(requestSeq, command)
        }

      def initialize(): Either[String, Unit] =
        if (initialized) {
          Right(())
        } else {
          val requestSeq = nextSeq()
          writeRequest(
            requestSeq,
            "initialize",
            Some(
              Json.obj(
                "clientID" -> Json.fromString("dap-http-generator"),
                "clientName" -> Json.fromString("dap-http-generator"),
                "adapterID" -> Json.fromString("dap-http-generator"),
                "pathFormat" -> Json.fromString("path"),
                "linesStartAt1" -> Json.True,
                "columnsStartAt1" -> Json.True,
                "supportsVariableType" -> Json.True,
                "supportsVariablePaging" -> Json.False,
                "supportsRunInTerminalRequest" -> Json.False
              )
            )
          )
          readUntilResponse(requestSeq, "initialize").flatMap { body =>
            writeEvent("initialized")
            val needsConfigurationDone = body.hcursor
              .downField("supportsConfigurationDoneRequest")
              .as[Boolean]
              .getOrElse(false)
            if (needsConfigurationDone) {
              val configSeq = nextSeq()
              writeRequest(configSeq, "configurationDone", None)
              // Mark initialized only after the full handshake succeeds so a failed
              // configurationDone does not leave a half-ready session that skips setup.
              readUntilResponse(configSeq, "configurationDone").map { _ =>
                initialized = true
                ()
              }
            } else {
              initialized = true
              Right(())
            }
          }
        }

      def trySendRequest(
          command: String,
          arguments: Option[Json],
          requestTimeoutMs: Int
      ): Option[Json] = {
        val previousTimeout = timeoutMs
        setTimeout(requestTimeoutMs)
        val result =
          try {
            sendRequest(command, arguments).toOption
          } catch {
            case _: java.net.SocketTimeoutException =>
              DapHttpLoggers.dap.debug(
                "DAP {} timed out after {} ms",
                command,
                Integer.valueOf(requestTimeoutMs)
              )
              None
          } finally {
            setTimeout(previousTimeout)
          }
        result
      }

      private def nextSeq(): Int = {
        val value = seqCounter
        seqCounter += 1
        value
      }

      private def writeRequest(seq: Int, command: String, arguments: Option[Json]): Unit = {
        val request = arguments match {
          case Some(args) =>
            Json.obj(
              "seq" -> Json.fromInt(seq),
              "type" -> Json.fromString("request"),
              "command" -> Json.fromString(command),
              "arguments" -> args
            )
          case None =>
            Json.obj(
              "seq" -> Json.fromInt(seq),
              "type" -> Json.fromString("request"),
              "command" -> Json.fromString(command)
            )
        }
        writeMessage(request)
      }

      private def writeEvent(event: String): Unit = {
        val payload = Json.obj(
          "seq" -> Json.fromInt(nextSeq()),
          "type" -> Json.fromString("event"),
          "event" -> Json.fromString(event)
        )
        writeMessage(payload)
      }

      private def writeMessage(json: Json): Unit = {
        val payload = json.noSpaces.getBytes(StandardCharsets.UTF_8)
        out.write(s"Content-Length: ${payload.length}\r\n\r\n".getBytes(StandardCharsets.UTF_8))
        out.write(payload)
        out.flush()
      }

      private def readUntilResponse(requestSeq: Int, command: String): Either[String, Json] = {
        var skippedEvents = 0
        while (skippedEvents < 64) {
          val body = readMessageBody()
          io.circe.parser.parse(body).toOption match {
            case Some(json) if isMatchingResponse(json, requestSeq) =>
              return parseDapResponse(json, command)
            case Some(json) if json.hcursor.downField("type").as[String].contains("event") =>
              DapHttpLoggers.dap.debug(
                "skipping DAP event {} while waiting for {} response",
                json.hcursor.downField("event").as[String].getOrElse("?"),
                command
              )
              skippedEvents += 1
            case Some(json) =>
              DapHttpLoggers.dap.debug(
                "skipping unexpected DAP message while waiting for {} response: {}",
                command,
                json.noSpaces
              )
              skippedEvents += 1
            case None =>
              return Left(s"Failed to parse DAP $command response payload.")
          }
        }
        Left(s"Timed out waiting for DAP $command response.")
      }

      private def isMatchingResponse(json: Json, requestSeq: Int): Boolean =
        json.hcursor.downField("type").as[String].contains("response") &&
          json.hcursor.downField("request_seq").as[Int].contains(requestSeq)

      private def readMessageBody(): String = {
        val contentLength = readContentLength(in)
        readBody(in, contentLength)
      }
    }

    private def parseDapResponse(json: Json, command: String): Either[String, Json] =
      if (json.hcursor.downField("success").as[Boolean].getOrElse(false)) {
        Right(json.hcursor.downField("body").focus.getOrElse(Json.Null))
      } else {
        val message = json.hcursor
          .downField("message")
          .as[String]
          .toOption
          .getOrElse(s"DAP $command failed")
        Left(message)
      }

    private def parseThreadIds(responseBody: Json): List[Int] =
      responseBody.hcursor
        .downField("threads")
        .values
        .getOrElse(Vector.empty)
        .flatMap(_.hcursor.downField("id").as[Int].toOption)
        .toList

    private def readContentLength(in: BufferedInputStream): Int = {
      var contentLength = 0
      var line = readLine(in)
      while (line.nonEmpty) {
        val lower = line.toLowerCase
        if (lower.startsWith("content-length:")) {
          contentLength = lower.stripPrefix("content-length:").trim.toInt
        }
        line = readLine(in)
      }
      contentLength
    }

    private def readBody(in: BufferedInputStream, length: Int): String = {
      val buffer = new Array[Byte](length)
      var read = 0
      while (read < length) {
        val bytesRead = in.read(buffer, read, length - read)
        if (bytesRead == -1)
          throw new IllegalStateException("Unexpected EOF while reading DAP response body.")
        read += bytesRead
      }
      new String(buffer, StandardCharsets.UTF_8)
    }

    private def readLine(in: BufferedInputStream): String = {
      val buffer = new StringBuilder
      var current = in.read()
      var previous = -1
      while (current != -1 && !(previous == '\r' && current == '\n')) {
        if (current != '\r') buffer.append(current.toChar)
        previous = current
        current = in.read()
      }
      buffer.toString()
    }
  }
}
