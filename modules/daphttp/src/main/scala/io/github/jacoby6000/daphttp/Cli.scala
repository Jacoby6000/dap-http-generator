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
import scala.util.Try

object Cli
    extends CommandIOApp(
      name = "dap-http-generator",
      header = "DAP HTTP generator – serve DAP memory reads over HTTP"
    ) {

  private final case class ServerConfig(
      dapPipe: Option[Path],
      dapHost: String,
      dapPort: Int,
      dapTimeoutMs: Int,
      dapContinueTimeoutMs: Int,
      dapConnectTimeoutMs: Int,
      dapConnectRetryMs: Int,
      bindHost: String,
      bindPort: Int,
      overlaysPath: Option[Path]
  )

  private val dataSectionsOpt: Opts[Set[String]] =
    Opts
      .option[String](
        "data-sections",
        "Comma-separated list of additional section names to scan for data symbols (e.g. .mydata,.custom)",
        metavar = "sections"
      )
      .map(_.split(',').map(_.trim).filter(_.nonEmpty).toSet)
      .withDefault(Set.empty)

  private val reportOpt: Opts[Option[Path]] =
    Opts
      .option[Path](
        "report",
        "Write a detailed cheaders diagnostics report (full skip/conflict lists) to this path",
        metavar = "path"
      )
      .orNone

  private val overlaysOpt: Opts[Option[Path]] =
    Opts
      .option[Path](
        "overlays",
        "Load/save client type reinterpretation overlays (JSON) at this path",
        metavar = "path"
      )
      .orNone

  private val serverConfigOpts: Opts[ServerConfig] = (
    Opts
      .option[Path](
        "dap-pipe",
        "Existing local DAP path: Unix domain socket, or \\\\.\\pipe\\Name on Windows",
        metavar = "path"
      )
      .orNone,
    Opts
      .option[String]("dap-host", "DAP debug adapter host (TCP)", metavar = "host")
      .withDefault("127.0.0.1"),
    Opts
      .option[Int]("dap-port", "DAP debug adapter port (TCP)", metavar = "port")
      .withDefault(4711),
    Opts
      .option[Int]("dap-timeout-ms", "DAP socket read timeout in milliseconds", metavar = "ms")
      .withDefault(5000),
    Opts
      .option[Int](
        "dap-continue-timeout-ms",
        "DAP socket read timeout for continue/resume in milliseconds",
        metavar = "ms"
      )
      .withDefault(30000),
    Opts
      .option[Int](
        "dap-connect-timeout-ms",
        "DAP TCP connect timeout per attempt in milliseconds",
        metavar = "ms"
      )
      .withDefault(1000),
    Opts
      .option[Int](
        "dap-connect-retry-ms",
        "Delay between DAP connect attempts in milliseconds",
        metavar = "ms"
      )
      .withDefault(5000),
    Opts
      .option[String]("bind-host", "HTTP server bind host", metavar = "host")
      .withDefault("0.0.0.0"),
    Opts
      .option[Int]("bind-port", "HTTP server bind port", metavar = "port")
      .withDefault(8080),
    overlaysOpt
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
        dataSectionsOpt,
        reportOpt,
        serverConfigOpts
      ).mapN {
        (
            symbolsPath,
            headerPaths,
            namespace,
            service,
            wordSize,
            dataSections,
            reportPath,
            serverConfig
        ) =>
          IO.blocking(
            loadCHeaderPlans(
              symbolsPath,
              headerPaths.toList,
              namespace,
              service,
              wordSize,
              dataSections,
              reportPath
            )
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
        dataSectionsOpt,
        reportOpt,
        Opts.option[Path]("output", "Output Smithy model file path", metavar = "path")
      ).mapN {
        (
            symbolsPath,
            headerPaths,
            namespace,
            service,
            wordSize,
            dataSections,
            reportPath,
            outputPath
        ) =>
          IO.blocking(
            emitSmithyFromCHeaders(
              symbolsPath,
              headerPaths.toList,
              namespace,
              service,
              wordSize,
              dataSections,
              outputPath,
              reportPath
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

  private def loadModel(paths: List[Path]): Either[List[String], Model] =
    SmithyModelLoader.load(paths)

  private def loadCHeaderPlans(
      symbolsPath: Path,
      headerPaths: List[Path],
      namespace: String,
      service: String,
      wordSize: Int,
      extraDataSections: Set[String],
      reportPath: Option[Path]
  ): RoutePlansLoadResult =
    loadIrFromCHeaders(
      symbolsPath,
      headerPaths,
      namespace,
      service,
      wordSize,
      extraDataSections
    ) match {
      case Left(errors) =>
        errors.foreach(error => DapHttpLoggers.irSourceDoldecomp.warn("{}", error))
        RoutePlansLoadResult(Map.empty, errors)
      case Right(generation) =>
        // DESNOTE(jbarber, 2026-07-20): DoldecompIrGenerator already logs its warnings while
        // building IR; re-logging here doubled every line on cheaders startup.
        writeReportIfRequested(reportPath, generation, symbolsPath)
        IrSizingWarnings.writeToStderr(generation.services)
        val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generation.services)
        RoutePlansLoadResult(
          routes = plans.routes,
          errors = generation.warnings ++ plans.errors,
          services = generation.services
        )
    }

  private def loadIrFromCHeaders(
      symbolsPath: Path,
      headerPaths: List[Path],
      namespace: String,
      service: String,
      wordSize: Int,
      extraDataSections: Set[String]
  ): Either[List[String], IrGenerationResult] =
    DoldecompIrGenerator.generateFromPaths(
      symbolsPath,
      headerPaths,
      namespace,
      service,
      wordSize,
      extraDataSections
    )

  private def emitSmithyFromCHeaders(
      symbolsPath: Path,
      headerPaths: List[Path],
      namespace: String,
      service: String,
      wordSize: Int,
      extraDataSections: Set[String],
      outputPath: Path,
      reportPath: Option[Path]
  ): Either[List[String], Unit] =
    loadIrFromCHeaders(symbolsPath, headerPaths, namespace, service, wordSize, extraDataSections)
      .flatMap { generation =>
        writeReportIfRequested(reportPath, generation, symbolsPath)
        val operations = generation.services.flatMap(_.operations)
        if (generation.services.isEmpty || operations.isEmpty) {
          val emptyOpsWarning =
            if (operations.isEmpty && generation.services.nonEmpty)
              List("No operations generated from C headers / symbols.")
            else Nil
          Left(generation.warnings ++ emptyOpsWarning)
        } else {
          IrSizingWarnings.writeToStderr(generation.services)
          generation.warnings.foreach(System.err.println)
          SmithyIrEmitter.emitToPath(generation.services, outputPath)
        }
      }

  private def writeReportIfRequested(
      reportPath: Option[Path],
      generation: IrGenerationResult,
      symbolsPath: Path
  ): Unit =
    reportPath.foreach { path =>
      val written =
        DoldecompReport.write(
          path,
          generation.diagnostics,
          generation.warnings,
          symbolsPath = Some(symbolsPath)
        )
      DapHttpLoggers.irSourceDoldecomp.info("Wrote diagnostics report to {}", written)
    }

  private def runServer(
      config: ServerConfig,
      plans: RoutePlansLoadResult,
      watchPaths: List[Path],
      watch: Boolean
  ): IO[ExitCode] =
    for {
      plansRef <- Ref.of[IO, RoutePlansLoadResult](plans)
      overlayDocument <- IO.blocking {
        config.overlaysPath match {
          case None =>
            TypeOverlayDocument.empty
          case Some(path) =>
            TypeOverlayDocument.load(path) match {
              case Right(doc) => doc
              case Left(err)  =>
                System.err.println(s"Failed to load overlays from $path: $err")
                TypeOverlayDocument.empty
            }
        }
      }
      overlaysRef <- Ref.of[IO, OverlayEngine](
        OverlayEngine.fromServices(overlayDocument, plans.services)
      )
      dapClient = DapClients.create(
        config.dapPipe,
        config.dapHost,
        config.dapPort,
        config.dapTimeoutMs,
        config.dapContinueTimeoutMs,
        config.dapConnectTimeoutMs,
        config.dapConnectRetryMs
      )
      watchService <- RealtimeWatchService.create(dapClient, plansRef, overlaysRef)
      _ <-
        if (watch && watchPaths.nonEmpty)
          startSmithyWatcher(watchPaths, plansRef, overlaysRef, watchService)
        else IO.unit
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
            DapHttpServerMain
              .routes(
                plansRef,
                dapClient,
                overlaysRef,
                config.overlaysPath,
                watchService,
                wsBuilder
              )
              .orNotFound
          )
        }
        .build
        .use(_ => IO.never)
        .as(ExitCode.Success)
    } yield exit

  private def startSmithyWatcher(
      paths: List[Path],
      plansRef: Ref[IO, RoutePlansLoadResult],
      overlaysRef: Ref[IO, OverlayEngine],
      watchService: RealtimeWatchService
  ): IO[Unit] = {
    def newestTimestamp: Long =
      paths
        .flatMap(SmithyModelLoader.collectSmithyFiles)
        .flatMap(path => Try(Files.getLastModifiedTime(path).toMillis).toOption)
        .sorted
        .lastOption
        .getOrElse(0L)

    def loop(lastSeen: Long): IO[Unit] =
      IO.sleep(2.seconds) *> IO.blocking(newestTimestamp).flatMap { newest =>
        if (newest > lastSeen)
          IO.blocking(loadSmithyPlans(paths)).flatMap { plans =>
            for {
              _ <- plansRef.set(plans)
              engine <- overlaysRef.get
              _ <- overlaysRef.set(OverlayEngine.fromServices(engine.document, plans.services))
              _ <- watchService.rebindAll
              _ <- loop(newest)
            } yield ()
          }
        else
          loop(lastSeen)
      }

    IO.blocking(newestTimestamp).flatMap(ts => loop(ts).start.void)
  }
}
