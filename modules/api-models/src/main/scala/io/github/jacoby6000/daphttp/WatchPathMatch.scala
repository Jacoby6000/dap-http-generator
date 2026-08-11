package io.github.jacoby6000.daphttp

/** Pure helpers for realtime watch path / overlay-segment matching in the explorer. */
object WatchPathMatch {
  def coversBase(watchedOrSource: String, basePath: String): Boolean =
    watchedOrSource == basePath || watchedOrSource.startsWith(basePath + "/")

  def coversSegments(fieldSegments: List[String], watchedSegments: List[String]): Boolean =
    fieldSegments == watchedSegments || fieldSegments.startsWith(watchedSegments)

  def isOverlaySegmentWatched(
      basePath: String,
      segments: List[String],
      activeWatchPaths: Iterable[String],
      overlaySegmentsBySource: Iterable[(String, List[List[String]])]
  ): Boolean =
    activeWatchPaths.exists(p => p == basePath) ||
      overlaySegmentsBySource.exists { case (sourcePath, overlaySegs) =>
        coversBase(sourcePath, basePath) &&
        overlaySegs.exists(segs => coversSegments(segments, segs))
      }
}
