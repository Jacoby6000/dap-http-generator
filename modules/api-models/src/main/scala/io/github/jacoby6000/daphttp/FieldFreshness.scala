package io.github.jacoby6000.daphttp

/** Stamp keys and age visuals for explorer dual-decode freshness styling. */
object FieldFreshness {
  val StaleAfterMs: Double = 60_000.0

  def jsonPointer(segments: List[String]): String =
    if (segments.isEmpty) "" else segments.mkString("/", "/", "")

  def stampKey(
      basePath: String,
      segments: List[String],
      overlayPanel: Boolean
  ): String = {
    val raw = basePath + jsonPointer(segments)
    if (overlayPanel) s"ov:$raw" else raw
  }

  def dualStampKey(
      basePath: String,
      sourceSegments: List[String],
      overlaySegments: List[String]
  ): String =
    stampKey(basePath, sourceSegments, overlayPanel = false) + "\n" +
      stampKey(basePath, overlaySegments, overlayPanel = true)

  /** Mild fade for the first minute; a clearer (but still readable) mute after that. */
  def ageVisual(ageMs: Double): (Double, Double) = {
    val sec = ageMs / 1000.0
    if (sec <= 0) (1.0, 0.0)
    else if (sec < 60.0) {
      val t = sec / 60.0
      (1.0 - t * 0.10, t * 0.22)
    } else {
      val extra = math.min(1.0, (sec - 60.0) / 180.0)
      (0.82 - extra * 0.06, 0.34 + extra * 0.08)
    }
  }

  def ageMs(freshMs: Double, latestDataTime: Double, nowMs: Double): Double = {
    val latest = math.max(latestDataTime, nowMs)
    math.max(0.0, latest - freshMs)
  }

  def resolveFreshMs(
      lookup: List[String] => Option[Double],
      segments: List[String],
      fallback: Double
  ): Double = {
    var segs = segments
    while (true) {
      lookup(segs) match {
        case Some(stamped) =>
          return stamped
        case None =>
          if (segs.isEmpty) return fallback
          segs = segs.init
      }
    }
    fallback
  }
}
