ThisBuild / scalaVersion := "2.13.14"
ThisBuild / organization := "io.github.jacoby6000"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val smithyVersion = "1.53.0"

lazy val root = (project in file("."))
  .settings(
    name := "dap-http-generator",
    libraryDependencies ++= Seq(
      "software.amazon.smithy" % "smithy-build" % smithyVersion,
      "software.amazon.smithy" % "smithy-model" % smithyVersion,
      "org.scalatest" %% "scalatest" % "3.2.19" % Test
    ),
    Test / fork := true
  )
