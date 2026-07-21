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

  def extractIndices(template: String, concrete: String): Option[List[Int]] = {
    val t = template.split('/')
    val c = concrete.split('/')
    if (t.length != c.length) None
    else {
      val pairs = t.zip(c)
      if (
        pairs.forall {
          case ("{index}", seg) => seg.nonEmpty && seg.forall(_.isDigit)
          case (a, b)           => a == b
        }
      )
        Some(pairs.collect { case ("{index}", seg) => seg.toInt }.toList)
      else None
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
          val nums = parts.reverse.takeWhile(s => s.nonEmpty && s.forall(_.isDigit)).reverse
          if (nums.isEmpty) None
          else {
            val base = parts.dropRight(nums.length).mkString("/")
            val template =
              (parts.dropRight(nums.length) ++ List.fill(nums.length)("{index}")).mkString("/")
            if (catalogTemplates.exists(t => t == template || t.startsWith(base + "/")))
              Some((template, nums.map(_.toInt)))
            else None
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
