ThisBuild / scalaVersion := "2.13.18"
ThisBuild / organization := "io.github.jacoby6000"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / scalafmtOnCompile := true
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint:_",
  "-Wdead-code",
  "-Wextra-implicit",
  "-Wnumeric-widen",
  "-Xlint:implicit-recursion",
  "-Wunused:imports,patvars,privates,locals,explicits,implicits",
  "-Wvalue-discard"
)

lazy val smithyVersion = "1.72.0"
lazy val declineVersion = "2.4.1"
lazy val circeVersion = "0.14.16"

lazy val apiModels = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/api-models"))
  .settings(
    name := "dap-http-api-models",
    // DESNOTE(jbarber, 2026-07-21): Wire models only — no IR, DAP, or UI state. Shared by
    // the JVM server and Scala.js explorer so /routes /overlays /types JSON stays in sync.
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core" % circeVersion,
      "io.circe" %%% "circe-parser" % circeVersion
    )
  )

lazy val apiModelsJVM = apiModels.jvm
lazy val apiModelsJS = apiModels.js

lazy val ui = project
  .in(file("modules/ui"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(apiModels.js)
  .settings(
    name := "dap-http-ui",
    scalaJSUseMainModuleInitializer := true,
    // DESNOTE(jbarber, 2026-07-18): Scala.js DOM / Promise interop is noisy under the JVM
    // module's strict -Xlint/-Wunused settings; keep those fatal warnings on the server.
    scalacOptions --= Seq(
      "-Xlint:_",
      "-Wunused:imports,patvars,privates,locals,explicits,implicits",
      "-Wvalue-discard"
    ),
    scalacOptions += "-P:scalajs:nowarnGlobalExecutionContext",
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
      "io.circe" %%% "circe-core" % circeVersion,
      "io.circe" %%% "circe-parser" % circeVersion,
      "io.circe" %%% "circe-generic" % circeVersion
    )
  )

lazy val root = (project in file("modules/daphttp"))
  .dependsOn(apiModels.jvm)
  .settings(
    name := "dap-http-generator",
    libraryDependencies ++= Seq(
      "software.amazon.smithy" % "smithy-model" % smithyVersion,
      "org.http4s" %% "http4s-ember-server" % "0.23.32",
      "org.http4s" %% "http4s-dsl" % "0.23.32",
      "org.http4s" %% "http4s-circe" % "0.23.32",
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "co.fs2" %% "fs2-core" % "3.13.0",
      "org.scodec" %% "scodec-core" % "1.11.10",
      "io.joern" % "eclipse-cdt-core" % "8.4.0.202401242025_1",
      "org.eclipse.platform" % "org.eclipse.core.runtime" % "3.33.100",
      "org.eclipse.platform" % "org.eclipse.core.resources" % "3.22.200",
      "com.monovore" %% "decline" % declineVersion,
      "com.monovore" %% "decline-effect" % declineVersion,
      "org.slf4j" % "slf4j-api" % "2.0.17",
      "ch.qos.logback" % "logback-classic" % "1.5.18",
      "org.scalatest" %% "scalatest" % "3.2.20" % Test
    ),
    Compile / mainClass := Some("io.github.jacoby6000.daphttp.Cli"),
    // DESNOTE(jbarber, 2026-07-20): Fork run so cats-effect IOApp owns the main thread (avoids the
    // non-main-thread cleanup warning) and so Melee-scale CDT parses get a dedicated heap.
    Compile / run / fork := true,
    Compile / run / javaOptions ++= Seq("-Xmx4g"),
    Test / fork := true,
    Compile / resourceGenerators += Def.task {
      val jsFile = (ui / Compile / fastOptJS).value.data
      val destDir = (Compile / resourceManaged).value / "web"
      IO.createDirectory(destDir)
      val destJs = destDir / "main.js"
      IO.copyFile(jsFile, destJs)
      val indexSrc = (ui / Compile / resourceDirectory).value / "index.html"
      val destHtml = destDir / "index.html"
      IO.copyFile(indexSrc, destHtml)
      Seq(destJs, destHtml)
    }.taskValue
  )

addCommandAlias("fmt", ";scalafmtAll;scalafmtSbt")
addCommandAlias("fix", ";scalafixAll")
