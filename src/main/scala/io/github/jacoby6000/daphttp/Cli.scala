package io.github.jacoby6000.daphttp

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all._
import com.comcast.ip4s.Host
import com.comcast.ip4s.Port
import com.monovore.decline.Command
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import software.amazon.smithy.model.Model

import java.nio.file.Files
import java.nio.file.Path
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

object Cli
    extends CommandIOApp(
      name = "dap-http-generator",
      header = "DAP HTTP generator – serve DAP memory reads over HTTP"
    ) {

  private final case class ServerConfig(
      dapHost: String,
      dapPort: Int,
      bindHost: String,
      bindPort: Int
  )

  private val serverConfigOpts: Opts[ServerConfig] = (
    Opts
      .option[String]("dap-host", "DAP debug adapter host", metavar = "host")
      .withDefault("127.0.0.1"),
    Opts
      .option[Int]("dap-port", "DAP debug adapter port", metavar = "port")
      .withDefault(4711),
    Opts
      .option[String]("bind-host", "HTTP server bind host", metavar = "host")
      .withDefault("0.0.0.0"),
    Opts
      .option[Int]("bind-port", "HTTP server bind port", metavar = "port")
      .withDefault(8080)
  ).mapN(ServerConfig.apply)

  private val smithySubcommand: Command[IO[ExitCode]] =
    Command(
      name = "smithy",
      header = "Load API definitions from Smithy model files"
    )(
      (
        Opts.options[Path]("smithy", "Smithy model file or directory paths", metavar = "path"),
        Opts.flag("watch", "Watch Smithy files for changes and reload").orFalse,
        serverConfigOpts
      ).mapN { (smithyPaths, watch, serverConfig) =>
        val paths = smithyPaths.toList
        IO.blocking(loadSmithyPlans(paths)).flatMap(runServer(serverConfig, _, paths, watch))
      }
    )

  private val cheadersSubcommand: Command[IO[ExitCode]] =
    Command(
      name = "cheaders",
      header = "Load API definitions from C header files and a doldecomp symbols file"
    )(
      (
        Opts.option[Path]("symbols", "doldecomp symbols file path", metavar = "path"),
        Opts.options[Path]("headers", "C header file or directory paths", metavar = "path"),
        Opts
          .option[String](
            "namespace",
            "Smithy namespace for generated types",
            metavar = "namespace"
          )
          .withDefault("doldecomp.generated"),
        Opts
          .option[String]("service", "Service name", metavar = "name")
          .withDefault("DolDecompApi"),
        Opts
          .option[Int]("word-size", "Pointer word size in bits", metavar = "bits")
          .withDefault(32),
        serverConfigOpts
      ).mapN { (symbolsPath, headerPaths, namespace, service, wordSize, serverConfig) =>
        IO.blocking(
          loadCHeaderPlans(symbolsPath, headerPaths.toList, namespace, service, wordSize)
        ).flatMap(runServer(serverConfig, _, Nil, watch = false))
      }
    )

  private val cheadersSmithySubcommand: Command[IO[ExitCode]] =
    Command(
      name = "cheaders-smithy",
      header = "Generate Smithy model files from C headers and a doldecomp symbols file"
    )(
      (
        Opts.option[Path]("symbols", "doldecomp symbols file path", metavar = "path"),
        Opts.options[Path]("headers", "C header file or directory paths", metavar = "path"),
        Opts
          .option[String](
            "namespace",
            "Smithy namespace for generated types",
            metavar = "namespace"
          )
          .withDefault("doldecomp.generated"),
        Opts
          .option[String]("service", "Service name", metavar = "name")
          .withDefault("DolDecompApi"),
        Opts
          .option[Int]("word-size", "Pointer word size in bits", metavar = "bits")
          .withDefault(32),
        Opts.option[Path]("output", "Output Smithy model file path", metavar = "path")
      ).mapN { (symbolsPath, headerPaths, namespace, service, wordSize, outputPath) =>
        IO.blocking(
          emitSmithyFromCHeaders(
            symbolsPath,
            headerPaths.toList,
            namespace,
            service,
            wordSize,
            outputPath
          )
        ).map {
          case Right(_) =>
            IO.println(s"Wrote Smithy model to $outputPath") *> IO.pure(ExitCode.Success)
          case Left(errors) =>
            IO.println(errors.mkString("\n")) *> IO.pure(ExitCode.Error)
        }.flatten
      }
    )

  override def main: Opts[IO[ExitCode]] =
    Opts.subcommand(smithySubcommand) orElse Opts.subcommand(cheadersSubcommand) orElse Opts
      .subcommand(cheadersSmithySubcommand)

  private def loadSmithyPlans(paths: List[Path]): RoutePlansLoadResult =
    loadModel(paths) match {
      case Left(errors) =>
        RoutePlansLoadResult(Map.empty, errors)
      case Right(model) =>
        DapHttpServerMain.buildRoutePlansFromModel(model)
    }

  private def loadModel(paths: List[Path]): Either[List[String], Model] = {
    val smithyFiles = paths.flatMap(collectSmithyFiles).distinct
    val assembler = Model.assembler()
    smithyFiles.foreach(path => assembler.addImport(path.toString))
    val result = assembler.assemble()
    if (result.isBroken) {
      Left(result.getValidationEvents.asScala.map(_.toString).toList)
    } else {
      Right(result.unwrap())
    }
  }

  private def loadCHeaderPlans(
      symbolsPath: Path,
      headerPaths: List[Path],
      namespace: String,
      service: String,
      wordSize: Int
  ): RoutePlansLoadResult =
    loadIrFromCHeaders(symbolsPath, headerPaths, namespace, service, wordSize) match {
      case Left(errors) =>
        RoutePlansLoadResult(Map.empty, errors)
      case Right(generation) =>
        IrSizingWarnings.writeToStderr(generation.services)
        val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generation.services)
        RoutePlansLoadResult(
          routes = plans.routes,
          errors = generation.warnings ++ plans.errors
        )
    }

  private def loadIrFromCHeaders(
      symbolsPath: Path,
      headerPaths: List[Path],
      namespace: String,
      service: String,
      wordSize: Int
  ): Either[List[String], IrGenerationResult] =
    DoldecompIrGenerator.generateFromPaths(
      symbolsPath,
      headerPaths,
      namespace,
      service,
      wordSize
    )

  private def emitSmithyFromCHeaders(
      symbolsPath: Path,
      headerPaths: List[Path],
      namespace: String,
      service: String,
      wordSize: Int,
      outputPath: Path
  ): Either[List[String], Unit] =
    loadIrFromCHeaders(symbolsPath, headerPaths, namespace, service, wordSize).flatMap {
      generation =>
        if (generation.services.isEmpty) {
          Left(generation.warnings)
        } else {
          IrSizingWarnings.writeToStderr(generation.services)
          generation.warnings.foreach(System.err.println)
          SmithyIrEmitter.emitToPath(generation.services, outputPath)
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

  private def runServer(
      config: ServerConfig,
      plans: RoutePlansLoadResult,
      watchPaths: List[Path],
      watch: Boolean
  ): IO[ExitCode] =
    for {
      plansRef <- Ref.of[IO, RoutePlansLoadResult](plans)
      _ <-
        if (watch && watchPaths.nonEmpty) startSmithyWatcher(watchPaths, plansRef)
        else IO.unit
      dapClient = new DapHttpServerMain.SocketDapClient(config.dapHost, config.dapPort)
      app = DapHttpServerMain.routes(plansRef, dapClient).orNotFound
      exit <- EmberServerBuilder
        .default[IO]
        .withHost(Host.fromString(config.bindHost).getOrElse(Host.fromString("0.0.0.0").get))
        .withPort(Port.fromInt(config.bindPort).getOrElse(Port.fromInt(8080).get))
        .withHttpApp(app)
        .build
        .use(_ => IO.never)
        .as(ExitCode.Success)
    } yield exit

  private def startSmithyWatcher(
      paths: List[Path],
      plansRef: Ref[IO, RoutePlansLoadResult]
  ): IO[Unit] = {
    def newestTimestamp: Long =
      paths
        .flatMap(collectSmithyFiles)
        .flatMap(path => Try(Files.getLastModifiedTime(path).toMillis).toOption)
        .sorted
        .lastOption
        .getOrElse(0L)

    def loop(lastSeen: Long): IO[Unit] =
      IO.sleep(2.seconds) *> IO.blocking(newestTimestamp).flatMap { newest =>
        if (newest > lastSeen)
          IO.blocking(loadSmithyPlans(paths)).flatMap(plansRef.set) *> loop(newest)
        else
          loop(lastSeen)
      }

    IO.blocking(newestTimestamp).flatMap(ts => loop(ts).start.void)
  }
}
