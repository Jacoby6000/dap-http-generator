package io.github.jacoby6000.daphttp

/** Pure helpers for `{index}` route templates used by the explorer index bar. */
object IndexPath {
  def countSlots(path: String): Int =
    path.split('/').count(_ == "{index}")

  def substitute(template: String, indices: List[Int]): String = {
    var remaining = indices
    template
      .split('/')
      .map {
        case "{index}" =>
          val v = remaining.headOption.getOrElse(0)
          remaining = remaining.drop(1)
          v.toString
        case other => other
      }
      .mkString("/")
  }

  // DESNOTE(jbarber, 2026-07-21): Digit-only strings can still overflow Int; use toIntOption
  // so oversized catalog/selection segments yield None instead of throwing.
  private def parseIndexSegment(seg: String): Option[Int] =
    if (seg.nonEmpty && seg.forall(_.isDigit)) seg.toIntOption
    else None

  def extractIndices(template: String, concrete: String): Option[List[Int]] = {
    val t = template.split('/')
    val c = concrete.split('/')
    if (t.length != c.length) None
    else {
      val indices = List.newBuilder[Int]
      val ok = t.zip(c).forall {
        case ("{index}", seg) =>
          parseIndexSegment(seg) match {
            case Some(n) =>
              indices += n
              true
            case None => false
          }
        case (a, b) => a == b
      }
      if (ok) Some(indices.result()) else None
    }
  }

  /** Resolve a catalog/selection path to an index template and starting values. */
  def resolveBrowse(
      path: String,
      catalogTemplates: List[String],
      currentTemplate: Option[String],
      currentValues: List[Int]
  ): Option[(String, List[Int])] =
    if (path.contains("{index}")) {
      val slots = countSlots(path)
      val values =
        if (currentTemplate.contains(path) && currentValues.length == slots) currentValues
        else List.fill(slots)(0)
      Some((path, values))
    } else {
      catalogTemplates.view
        .flatMap(t => extractIndices(t, path).map(t -> _))
        .headOption
        .orElse {
          val parts = path.split('/').toList
          val nums = parts.reverse.takeWhile(s => parseIndexSegment(s).isDefined).reverse
          if (nums.isEmpty) None
          else {
            val parsed = nums.flatMap(parseIndexSegment)
            if (parsed.length != nums.length) None
            else {
              val base = parts.dropRight(nums.length).mkString("/")
              val template =
                (parts.dropRight(nums.length) ++ List.fill(nums.length)("{index}")).mkString("/")
              if (catalogTemplates.exists(t => t == template || t.startsWith(base + "/")))
                Some((template, parsed))
              else None
            }
          }
        }
    }

  def concretePath(
      path: String,
      currentTemplate: Option[String],
      currentValues: List[Int]
  ): String =
    if (!path.contains("{index}")) path
    else
      currentTemplate
        .filter(_ == path)
        .map(t => substitute(t, currentValues))
        .getOrElse(path)
}
