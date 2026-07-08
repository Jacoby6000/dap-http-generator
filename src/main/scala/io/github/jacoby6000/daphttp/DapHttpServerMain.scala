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
import org.http4s.Method.POST
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
      dapTimeoutMs: Int,
      dapContinueTimeoutMs: Int,
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
          dapClient = new SocketDapClient(
            config.dapHost,
            config.dapPort,
            config.dapTimeoutMs,
            config.dapContinueTimeoutMs
          )
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
          dapTimeoutMs =
            values.get("dapTimeoutMs").flatMap(v => Try(v.toInt).toOption).getOrElse(5000),
          dapContinueTimeoutMs = values
            .get("dapContinueTimeoutMs")
            .flatMap(v => Try(v.toInt).toOption)
            .getOrElse(30000),
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

      case POST -> Root / "resume" =>
        dapClient.continueExecution().flatMap {
          case Right(response) =>
            Ok(Json.obj("status" -> Json.fromString("ok"), "dap" -> response))
          case Left(error) =>
            InternalServerError(Json.obj("error" -> Json.fromString(error)))
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
    def continueExecution(): IO[Either[String, Json]]
  }

  private[daphttp] final class SocketDapClient(
      host: String,
      port: Int,
      dapTimeoutMs: Int = 5000,
      dapContinueTimeoutMs: Int = 30000
  ) extends DapClient {
    private val connectionLock = new AnyRef
    private var session: Option[DapSocketSession] = None

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
      session.filter(_.isOpen) match {
        case Some(activeSession) =>
          activeSession.setTimeout(timeoutMs)
          activeSession
        case None =>
          DapHttpLoggers.dap.info(
            "connecting DAP session host={} port={}",
            host,
            Integer.valueOf(port)
          )
          val socket = new Socket(host, port)
          socket.setSoTimeout(timeoutMs)
          val activeSession = new DapSocketSession(
            socket,
            new BufferedOutputStream(socket.getOutputStream),
            new BufferedInputStream(socket.getInputStream),
            timeoutMs
          )
          session = Some(activeSession)
          activeSession
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
        ensureInitialized().flatMap { _ =>
          val requestSeq = nextSeq()
          writeRequest(requestSeq, command, arguments)
          readUntilResponse(requestSeq, command)
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

      private def ensureInitialized(): Either[String, Unit] =
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
            initialized = true
            val needsConfigurationDone = body.hcursor
              .downField("supportsConfigurationDoneRequest")
              .as[Boolean]
              .getOrElse(false)
            if (needsConfigurationDone) {
              val configSeq = nextSeq()
              writeRequest(configSeq, "configurationDone", None)
              readUntilResponse(configSeq, "configurationDone").map(_ => ())
            } else {
              Right(())
            }
          }
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
