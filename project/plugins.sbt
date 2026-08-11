addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.6.1")
// DESNOTE(jbarber, 2026-07-21): 0.14.6/0.14.7 intermittently fail `scalafixAll`
// with "Unable to load symbol table: …/resource.ext.<hash>.tmp" while sbt copies
// test resources (fixtures like symbols.txt). Confirmed workaround: stay on
// 0.14.5 until https://github.com/scalacenter/scalafix/issues/2469 is fixed.
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.14.5")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.21.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
