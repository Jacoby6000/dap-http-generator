package io.github.jacoby6000.daphttp

import cats.effect.{ExitCode, IO, IOApp, Ref}
import io.circe.Json
import io.circe.syntax._
import com.comcast.ip4s.{Host, Port}
import org.http4s.Method.GET
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.HttpRoutes
import software.amazon.smithy.model.Model

import java.io.{BufferedInputStream, BufferedOutputStream}
import java.net.Socket
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.Base64
import scala.jdk.CollectionConverters._
import scala.concurrent.duration._
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
          plansRef <- Ref.of[IO, Either[List[String], Map[String, RoutePlan]]](
            loadPlans(config.smithyPaths)
          )
          _ <- watchSmithySources(config, plansRef)
          dapClient = new SocketDapClient(config.dapHost, config.dapPort)
          app = routes(plansRef, dapClient).orNotFound
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

  private def routes(
      plansRef: Ref[IO, Either[List[String], Map[String, RoutePlan]]],
      dapClient: DapClient
  ): HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case GET -> Root / "health" =>
        Ok(Json.obj("status" -> Json.fromString("ok")))

      case GET -> Root / "routes" =>
        plansRef.get.flatMap {
          case Left(errors) =>
            Ok(Json.obj("errors" -> errors.asJson))
          case Right(routesMap) =>
            Ok(Json.obj("routes" -> routesMap.keys.toList.sorted.asJson))
        }

      case request @ GET -> _ =>
        val routePath = request.uri.path.renderString
        plansRef.get.flatMap {
          case Left(errors) =>
            InternalServerError(Json.obj("errors" -> errors.asJson))
          case Right(routesMap) =>
            routesMap.get(routePath) match {
              case None =>
                NotFound(Json.obj("error" -> Json.fromString(s"No route generated for $routePath")))
              case Some(routePlan) =>
                routePlan.reads
                  .foldLeft(IO.pure(List.empty[Json])) { (accIO, readPlan) =>
                    for {
                      acc <- accIO
                      read <- dapClient.readMemory(readPlan.address, readPlan.sizeBytes)
                    } yield {
                      val readJson = read match {
                        case Right(data) =>
                          Json.obj(
                            "path" -> Json.fromString(readPlan.path),
                            "address" -> Json.fromString(f"0x${readPlan.address}%x"),
                            "bytes" -> Json.fromInt(readPlan.sizeBytes),
                            "data" -> Json.fromString(data)
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

  private def loadPlans(smithyPaths: List[Path]): Either[List[String], Map[String, RoutePlan]] = {
    loadModel(smithyPaths).flatMap(buildRoutePlansFromModel)
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

  def buildRoutePlansFromModel(model: Model): Either[List[String], Map[String, RoutePlan]] = {
    IrExtractor.buildIrFromModel(model).flatMap(IrCompiler.compileRoutePlansFromIr)
  }

  private def watchSmithySources(
      config: Config,
      plansRef: Ref[IO, Either[List[String], Map[String, RoutePlan]]]
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

  private trait DapClient {
    def readMemory(address: Long, sizeBytes: Int): IO[Either[String, String]]
  }

  private final class SocketDapClient(host: String, port: Int) extends DapClient {
    override def readMemory(address: Long, sizeBytes: Int): IO[Either[String, String]] =
      IO.blocking {
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
            Right(value)
          case Some(json) =>
            Left(
              json.hcursor
                .downField("message")
                .as[String]
                .toOption
                .getOrElse("DAP readMemory failed")
            )
          case None =>
            Left("Failed to parse DAP response payload.")
        }
      }.handleError(error => Left(error.getMessage))

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
