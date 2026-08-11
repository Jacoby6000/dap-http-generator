package io.github.jacoby6000.daphttp

import scala.collection.mutable

/** First-definition-wins merge for macros, structs, typedefs, and enums across scanned files. */
private[daphttp] final case class MergedNamedMap[A](
    values: Map[String, A],
    warnings: List[String],
    conflicts: List[NamedConflict] = Nil
)

private[daphttp] object CheadersNamedMerge {

  def mergeNamedEntries[A](
      entries: List[(String, A, String)],
      kind: String,
      equivalent: (A, A) => Boolean
  ): MergedNamedMap[A] = {
    val conflictIgnored = mutable.LinkedHashMap.empty[String, mutable.ListBuffer[String]]
    val keptSource = mutable.LinkedHashMap.empty[String, String]
    val merged = mutable.LinkedHashMap.empty[String, A]
    entries.foreach { case (name, value, source) =>
      merged.get(name) match {
        case None =>
          merged(name) = value
          keptSource(name) = source
        case Some(existing) if equivalent(existing, value) =>
          ()
        case Some(_) =>
          conflictIgnored.getOrElseUpdate(name, mutable.ListBuffer.empty).append(source)
      }
    }
    val conflicts = conflictIgnored.toList.map { case (name, ignored) =>
      NamedConflict(
        name = name,
        keptSource = keptSource.getOrElse(name, "<unknown>"),
        ignoredSources = ignored.toList.distinct
      )
    }
    MergedNamedMap(
      merged.toMap,
      summarizeNameConflicts(kind, conflicts.map(_.name)),
      conflicts
    )
  }

  def summarizeNameConflicts(kind: String, names: List[String]): List[String] =
    if (names.isEmpty) {
      Nil
    } else {
      val sample = names.take(20).mkString(", ")
      val suffix = if (names.size > 20) s", … (${names.size - 20} more)" else ""
      List(
        s"Conflicting $kind definitions for ${names.size} name(s); keeping first: $sample$suffix."
      )
    }
}
