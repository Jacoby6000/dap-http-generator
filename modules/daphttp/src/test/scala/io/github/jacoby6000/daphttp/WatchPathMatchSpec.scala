package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite

final class WatchPathMatchSpec extends AnyFunSuite {
  test("coversBase matches exact and child paths") {
    assert(WatchPathMatch.coversBase("/api/a", "/api/a"))
    assert(WatchPathMatch.coversBase("/api/a/0", "/api/a"))
    assert(!WatchPathMatch.coversBase("/api/ab", "/api/a"))
  }

  test("coversSegments matches equal or nested field paths") {
    assert(WatchPathMatch.coversSegments(List("x", "y"), List("x", "y")))
    assert(WatchPathMatch.coversSegments(List("x", "y", "z"), List("x", "y")))
    assert(!WatchPathMatch.coversSegments(List("x"), List("x", "y")))
  }

  test("isOverlaySegmentWatched honors active watches and overlay mappings") {
    assert(
      WatchPathMatch.isOverlaySegmentWatched(
        "/api/root",
        List("a"),
        activeWatchPaths = List("/api/root"),
        overlaySegmentsBySource = Nil
      )
    )
    assert(
      WatchPathMatch.isOverlaySegmentWatched(
        "/api/root",
        List("ov", "f"),
        activeWatchPaths = Nil,
        overlaySegmentsBySource = List("/api/root/x" -> List(List("ov", "f")))
      )
    )
    assert(
      !WatchPathMatch.isOverlaySegmentWatched(
        "/api/root",
        List("other"),
        activeWatchPaths = Nil,
        overlaySegmentsBySource = List("/api/root/x" -> List(List("ov", "f")))
      )
    )
  }
}
