package io.github.jacoby6000.daphttp

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Ref
import com.comcast.ip4s.Host
import com.comcast.ip4s.Port
import io.circe.Json
import io.circe.syntax._
import org.http4s.HttpRoutes
import org.http4s.Method.GET
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import scodec.bits.BitVector
import software.amazon.smithy.model.Model

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
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
      dapHost: String,
      dapPort: Int,
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
          dapClient = new SocketDapClient(config.dapHost, config.dapPort)
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
          dapHost = values.getOrElse("dapHost", "127.0.0.1"),
          dapPort = values.get("dapPort").flatMap(v => Try(v.toInt).toOption).getOrElse(4711),
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
              "routes" -> result.routes.keys.toList.sorted.asJson,
              "errors" -> result.errors.asJson
            )
          )
        }

      case request @ GET -> _ =>
        val routePath = request.uri.path.renderString
        plansRef.get.flatMap { result =>
          result.routes.get(routePath) match {
            case None =>
              NotFound(Json.obj("error" -> Json.fromString(s"No route generated for $routePath")))
            case Some(routePlan) =>
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
                          "address" -> Json.fromString(f"0x${readPlan.address}%x"),
                          "bytes" -> Json.fromInt(readPlan.sizeBytes),
                          "data" -> Json.fromString(data),
                          "decoded" -> decoded
                        )
                      case Left(error) =>
                        Json.obj(
                          "path" -> Json.fromString(readPlan.path),
                          "address" -> Json.fromString(f"0x${readPlan.address}%x"),
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
          }
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
    val assembler = Model.assembler()
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
  ): IO[Json] = {
    if (readPlan.cStringPointer) {
      decodeCStringPointer(readPlan, base64Data, dapClient)
    } else {
      IO.pure {
        readPlan.decodeCodec match {
          case None        => Json.Null
          case Some(codec) =>
            Try(Base64.getDecoder.decode(base64Data)).toOption
              .flatMap(bytes => codec.decode(BitVector(bytes)).toOption.map(_.value))
              .getOrElse(Json.Null)
        }
      }
    }
  }

  private def pointerValue(bytes: Array[Byte], endian: IrEndian): Long = {
    val ordered = endian match {
      case IrEndian.Big    => bytes
      case IrEndian.Little => bytes.reverse
    }
    ordered.foldLeft(0L) { (acc, byte) =>
      (acc << 8) | (byte.toLong & 0xffL)
    }
  }

  private def decodeCStringPointer(
      readPlan: ReadPlan,
      base64Data: String,
      dapClient: DapClient
  ): IO[Json] = {
    val pointer = Try(Base64.getDecoder.decode(base64Data)).toOption.map(bytes =>
      pointerValue(bytes, readPlan.endian)
    )
    pointer match {
      case None          => IO.pure(Json.Null)
      case Some(address) =>
        def readChars(currentAddress: Long, acc: Vector[Byte]): IO[Vector[Byte]] =
          dapClient.readMemory(currentAddress, 1).flatMap {
            case Right(data) =>
              Try(Base64.getDecoder.decode(data)).toOption match {
                case Some(bytes) if bytes.nonEmpty && bytes.head != 0 =>
                  readChars(currentAddress + 1, acc :+ bytes.head)
                case Some(_) =>
                  IO.pure(acc)
                case None =>
                  IO.pure(acc)
              }
            case Left(_) =>
              IO.pure(acc)
          }

        readChars(address, Vector.empty).map(bytes =>
          Json.fromString(new String(bytes.toArray, StandardCharsets.US_ASCII))
        )
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
  }

  private[daphttp] final class SocketDapClient(host: String, port: Int) extends DapClient {
    override def readMemory(address: Long, sizeBytes: Int): IO[Either[String, String]] =
      IO.blocking {
        DapHttpLoggers.dap.debug(
          "readMemory host={} port={} address=0x{} bytes={}",
          host,
          Integer.valueOf(port),
          java.lang.Long.toHexString(address),
          Integer.valueOf(sizeBytes)
        )

        val socket = new Socket(host, port)
        socket.setSoTimeout(5000)
        val out = new BufferedOutputStream(socket.getOutputStream)
        val in = new BufferedInputStream(socket.getInputStream)

        val request =
          Json
            .obj(
              "seq" -> Json.fromInt(1),
              "type" -> Json.fromString("request"),
              "command" -> Json.fromString("readMemory"),
              "arguments" -> Json.obj(
                "memoryReference" -> Json.fromString(f"0x$address%x"),
                "count" -> Json.fromInt(sizeBytes)
              )
            )
            .noSpaces

        val payload = request.getBytes(StandardCharsets.UTF_8)
        out.write(s"Content-Length: ${payload.length}\r\n\r\n".getBytes(StandardCharsets.UTF_8))
        out.write(payload)
        out.flush()

        val contentLength = readContentLength(in)
        val body = readBody(in, contentLength)

        socket.close()

        io.circe.parser.parse(body).toOption match {
          case Some(json) if json.hcursor.downField("success").as[Boolean].getOrElse(false) =>
            val value = json.hcursor
              .downField("body")
              .downField("data")
              .as[String]
              .toOption
              .getOrElse(Base64.getEncoder.encodeToString(body.getBytes(StandardCharsets.UTF_8)))
            DapHttpLoggers.dap.debug(
              "readMemory address=0x{} succeeded bytes={}",
              java.lang.Long.toHexString(address),
              Integer.valueOf(sizeBytes)
            )
            Right(value)
          case Some(json) =>
            val message = json.hcursor
              .downField("message")
              .as[String]
              .toOption
              .getOrElse("DAP readMemory failed")
            DapHttpLoggers.dap.warn(
              "readMemory address=0x{} failed: {}",
              java.lang.Long.toHexString(address),
              message
            )
            Left(message)
          case None =>
            DapHttpLoggers.dap.warn(
              "readMemory address=0x{} failed: unable to parse DAP response payload",
              java.lang.Long.toHexString(address)
            )
            Left("Failed to parse DAP response payload.")
        }
      }.handleError { error =>
        DapHttpLoggers.dap.warn(
          "readMemory address=0x{} failed: {}",
          java.lang.Long.toHexString(address),
          error.getMessage
        )
        Left(error.getMessage)
      }

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
