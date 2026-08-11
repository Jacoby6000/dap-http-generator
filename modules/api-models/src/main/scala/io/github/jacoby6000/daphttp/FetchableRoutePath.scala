package io.github.jacoby6000.daphttp

/** Resolve explorer JSON field segments to a fetchable HTTP data path. */
object FetchableRoutePath {
  def httpPathForField(
      basePath: String,
      segments: List[String],
      fetchable: Set[String]
  ): Option[String] = {
    if (segments.exists(_.startsWith("_"))) None
    else {
      val path =
        if (segments.isEmpty) basePath
        else segments.foldLeft(basePath)((p, s) => s"$p/$s")
      if (isFetchable(path, fetchable)) Some(path)
      else if (nestedUnderFetchableAncestor(basePath, segments, fetchable)) Some(path)
      else None
    }
  }

  def isFetchable(path: String, fetchable: Set[String]): Boolean =
    fetchable.contains(path) ||
      fetchable.exists(template =>
        template.contains("{index}") && IndexPath.extractIndices(template, path).isDefined
      )

  private def nestedUnderFetchableAncestor(
      basePath: String,
      segments: List[String],
      fetchable: Set[String]
  ): Boolean =
    segments.inits.toList
      .drop(1)
      .exists { prefix =>
        prefix.nonEmpty && {
          val ancestor = prefix.foldLeft(basePath)((p, s) => s"$p/$s")
          isFetchable(ancestor, fetchable)
        }
      }
}
