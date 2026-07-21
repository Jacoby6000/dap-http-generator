package io.github.jacoby6000.daphttp

import java.nio.file.Files
import java.nio.file.Path
import scala.jdk.CollectionConverters._

/** Raw + CDT-prepared header/source corpus for the cheaders frontend. */
private[daphttp] final case class HeaderFile(path: Path, raw: String, cdtSource: String) {
  def isHeader: Boolean = path.toString.endsWith(".h")
  def isCSource: Boolean = path.toString.endsWith(".c")
}

private[daphttp] final case class HeaderCorpus(files: Vector[HeaderFile])

private[daphttp] object CheadersCorpus {

  def load(headerRoots: List[Path]): HeaderCorpus = {
    val paths = headerRoots.flatMap(collectSourceFiles).distinct.sortBy(_.toString)
    HeaderCorpus(
      paths.map { path =>
        val raw = new String(Files.readAllBytes(path))
        val isC = path.toString.endsWith(".c")
        HeaderFile(path, raw, CHeaderParser.prepareCdtSource(raw, neutralizeHeavyContent = isC))
      }.toVector
    )
  }

  def loadStructMemberOffsets(corpus: HeaderCorpus): Map[(String, String), Int] =
    // Offset comments must be read from raw source — stripped CDT input removes them.
    corpus.files.flatMap(file => CHeaderOffsetParser.parse(file.raw).toList).toMap

  def collectSourceFiles(root: Path): List[Path] =
    if (!Files.exists(root)) {
      Nil
    } else if (Files.isRegularFile(root) && isSourceFile(root)) {
      List(root)
    } else if (Files.isDirectory(root)) {
      val stream = Files.walk(root)
      try {
        stream
          .iterator()
          .asScala
          .filter(path => Files.isRegularFile(path) && isSourceFile(path))
          .toList
          .sortBy(_.toString)
      } finally {
        stream.close()
      }
    } else {
      Nil
    }

  def isSourceFile(path: Path): Boolean = {
    val name = path.toString
    name.endsWith(".h") || name.endsWith(".c")
  }
}
