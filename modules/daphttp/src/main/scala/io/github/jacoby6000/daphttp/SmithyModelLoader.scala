package io.github.jacoby6000.daphttp

import software.amazon.smithy.model.Model

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.jdk.CollectionConverters._

/** Shared Smithy model assembly for CLI and HTTP server bootstrap. */
private[daphttp] object SmithyModelLoader {

  /** Prefer the project-base path (sbt fork CWD), then repo-root relative after the modules move.
    */
  def traitsPath: Path = {
    val candidates = List(
      Paths.get("src/main/smithy/dap-http-traits.smithy"),
      Paths.get("modules/daphttp/src/main/smithy/dap-http-traits.smithy")
    )
    candidates
      .find(Files.exists(_))
      .orElse(
        Option(getClass.getResource("/smithy/dap-http-traits.smithy"))
          .map(url => Paths.get(url.toURI))
      )
      .getOrElse(candidates.head)
  }

  def collectSmithyFiles(path: Path): List[Path] =
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

  def load(paths: List[Path]): Either[List[String], Model] = {
    val smithyFiles = paths.flatMap(collectSmithyFiles).distinct
    val traits = traitsPath
    val assembler = Model.assembler()
    if (Files.exists(traits)) {
      assembler.addImport(traits.toString)
    }
    smithyFiles.foreach(path => assembler.addImport(path.toString))
    val result = assembler.assemble()
    if (result.isBroken) {
      Left(result.getValidationEvents.asScala.map(_.toString).toList)
    } else {
      Right(result.unwrap())
    }
  }
}
