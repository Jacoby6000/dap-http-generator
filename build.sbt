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

lazy val root = (project in file("."))
  .settings(
    name := "dap-http-generator",
    libraryDependencies ++= Seq(
      "software.amazon.smithy" % "smithy-model" % smithyVersion,
      "org.http4s" %% "http4s-ember-server" % "0.23.32",
      "org.http4s" %% "http4s-dsl" % "0.23.32",
      "org.http4s" %% "http4s-circe" % "0.23.32",
      "io.circe" %% "circe-generic" % "0.14.16",
      "io.circe" %% "circe-parser" % "0.14.16",
      "co.fs2" %% "fs2-core" % "3.13.0",
      "org.scodec" %% "scodec-core" % "1.11.10",
      "io.joern" % "eclipse-cdt-core" % "8.4.0.202401242025_1",
      "org.eclipse.platform" % "org.eclipse.core.runtime" % "3.33.100",
      "org.eclipse.platform" % "org.eclipse.core.resources" % "3.22.200",
      "org.scalatest" %% "scalatest" % "3.2.20" % Test
    ),
    Compile / mainClass := Some("io.github.jacoby6000.daphttp.DapHttpServerMain"),
    Test / fork := true
  )

addCommandAlias("fmt", ";scalafmtAll;scalafmtSbt")
addCommandAlias("fix", ";scalafixAll")
