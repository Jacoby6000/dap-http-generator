package io.github.jacoby6000.daphttp.ui

import io.circe.Json
import io.circe.parser.parse
import io.github.jacoby6000.daphttp.DecodedPayload
import io.github.jacoby6000.daphttp.JsonPath
import io.github.jacoby6000.daphttp.WatchPathMatch
import org.scalajs.dom
import org.scalajs.dom.HTMLElement

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

/** Realtime watch socket, subscribe/cancel, and memoryChanged payload patching.
  *
  * Mixed into [[Main]]; dual-view patching hooks remain on the host.
  */
private[ui] trait WatchClientSupport {
  protected def payloads: scala.collection.mutable.Map[String, Json]
  protected def activeWatches: scala.collection.mutable.Map[String, Int]
  protected def watchOverlaySegments: scala.collection.mutable.Map[String, List[List[String]]]
  protected def watchSocket: Option[dom.WebSocket]
  protected def watchSocket_=(value: Option[dom.WebSocket]): Unit
  protected def detailViewPath: Option[String]
  protected def postJson(path: String, body: Json): Future[Json]
  protected def deleteJson(path: String): Future[Json]
  protected def setIndexStatus(message: String, ok: Boolean): Unit
  protected def showDetail(path: String, json: Json): Unit
  protected def patchJsonViews(basePath: String, focusSegments: List[String] = Nil): Unit
  protected def bumpRefreshFreshnessAt(
      basePath: String,
      segments: List[String],
      overlayPanel: Boolean
  ): Unit

  protected def isOverlaySegmentWatched(basePath: String, segments: List[String]): Boolean =
    WatchPathMatch.isOverlaySegmentWatched(
      basePath,
      segments,
      activeWatches.keys,
      watchOverlaySegments
    )

  protected def toggleWatch(httpPath: String): Unit =
    activeWatches.get(httpPath) match {
      case Some(watchId) =>
        deleteJson(s"/watches/$watchId").onComplete {
          case Success(_) =>
            activeWatches.remove(httpPath)
            watchOverlaySegments.remove(httpPath)
            detailViewPath.foreach(p => payloads.get(p).foreach(showDetail(p, _)))
            setIndexStatus(s"Stopped watching $httpPath", ok = true)
          case Failure(err) =>
            setIndexStatus(err.getMessage, ok = false)
        }
      case None =>
        postJson("/watches", Json.obj("path" -> Json.fromString(httpPath))).onComplete {
          case Success(json) =>
            json.hcursor.get[Int]("watchId") match {
              case Right(watchId) =>
                activeWatches.update(httpPath, watchId)
                val overlaySegs = json.hcursor
                  .downField("overlaySegments")
                  .as[List[List[String]]]
                  .getOrElse(Nil)
                if (overlaySegs.nonEmpty) watchOverlaySegments.update(httpPath, overlaySegs)
                else watchOverlaySegments.remove(httpPath)
                detailViewPath.foreach(p => payloads.get(p).foreach(showDetail(p, _)))
                val overlayNote =
                  if (overlaySegs.isEmpty) ""
                  else s" (overlay: ${overlaySegs.map(_.mkString("/")).mkString(", ")})"
                setIndexStatus(s"Watching $httpPath (#$watchId)$overlayNote", ok = true)
              case Left(err) =>
                val msg = json.hcursor
                  .get[String]("error")
                  .toOption
                  .getOrElse(err.message)
                setIndexStatus(s"Watch failed: $msg", ok = false)
            }
          case Failure(err) =>
            setIndexStatus(err.getMessage, ok = false)
        }
    }

  protected def connectWatchSocket(): Unit = {
    watchSocket.foreach { ws =>
      try ws.close()
      catch { case _: Throwable => () }
    }
    val proto = if (dom.window.location.protocol == "https:") "wss:" else "ws:"
    val ws = new dom.WebSocket(s"$proto//${dom.window.location.host}/ws")
    watchSocket = Some(ws)
    ws.onmessage = { (event: dom.MessageEvent) =>
      parse(event.data.toString) match {
        case Right(json) => handleWatchSocketMessage(json)
        case Left(_)     => ()
      }
    }
    ws.onclose = { (_: dom.CloseEvent) =>
      watchSocket = None
      dom.window.setTimeout(() => connectWatchSocket(), 2000.0)
    }
    ws.onerror = { (_: dom.Event) =>
      try ws.close()
      catch { case _: Throwable => () }
    }
  }

  protected def handleWatchSocketMessage(json: Json): Unit = {
    val cursor = json.hcursor
    cursor.get[String]("type").toOption match {
      case Some("watchesCleared") =>
        activeWatches.clear()
        watchOverlaySegments.clear()
        detailViewPath.foreach(p => payloads.get(p).foreach(showDetail(p, _)))
        setIndexStatus("DAP reconnected — watches cleared", ok = false)
      case Some("watchesRebound") =>
        syncWatchesFromJsonList(json.hcursor.downField("watches").as[List[Json]].getOrElse(Nil))
        setIndexStatus("Watches rebound after overlay/model reload", ok = true)
      case Some("memoryChanged") =>
        val pathOpt = cursor.get[String]("path").toOption
        val decodedOpt = cursor.downField("decoded").focus
        val overlayOpt = cursor.downField("overlayDecoded").focus.filterNot(_.isNull)
        val overlayUpdates = cursor
          .downField("overlayUpdates")
          .as[List[Json]]
          .toOption
          .getOrElse(Nil)
          .flatMap { item =>
            for {
              segs <- item.hcursor.get[List[String]]("segments").toOption
              value <- item.hcursor.downField("decoded").focus
            } yield segs -> value
          }
        (pathOpt, decodedOpt) match {
          case (Some(watchedPath), Some(decoded)) =>
            applyWatchUpdate(watchedPath, decoded, overlayOpt, overlayUpdates)
          case _ =>
            ()
        }
      case _ =>
        ()
    }
  }

  protected def applyWatchUpdate(
      watchedPath: String,
      decoded: Json,
      overlay: Option[Json],
      overlayUpdates: List[(List[String], Json)]
  ): Unit = {
    val parents =
      payloads.keys.filter(base => watchedPath == base || watchedPath.startsWith(base + "/")).toList
    parents.foreach { basePath =>
      payloads.get(basePath).foreach { parent =>
        val segments =
          if (watchedPath == basePath) Nil
          else watchedPath.stripPrefix(basePath + "/").split("/").toList.filter(_.nonEmpty)
        val mergedDecoded =
          JsonPath.replace(DecodedPayload.extractDecoded(parent), segments, decoded)
        // DESNOTE(jbarber, 2026-07-20): Member watches send overlayUpdates without a full
        // overlayDecoded payload. Seed an empty object when needed so byte-mapped overlay
        // fields still patch in realtime (previously `.map` dropped updates when the parent
        // had no overlayDecoded yet).
        val baseOverlay =
          (
            DecodedPayload.extractOverlayDecoded(parent).filterNot(_.isNull),
            overlay.filterNot(_.isNull)
          ) match {
            case (_, Some(piece)) if segments.isEmpty =>
              Some(piece)
            case (Some(rootOverlay), Some(piece)) =>
              Some(JsonPath.replace(rootOverlay, segments, piece))
            case (Some(rootOverlay), None) =>
              Some(rootOverlay)
            case (None, Some(piece)) =>
              Some(JsonPath.replace(Json.obj(), segments, piece))
            case (None, None) if overlayUpdates.nonEmpty =>
              Some(Json.obj())
            case _ =>
              None
          }
        val mergedOverlay =
          overlayUpdates.foldLeft(baseOverlay) { case (acc, (overlaySegs, value)) =>
            Some(JsonPath.replace(acc.getOrElse(Json.obj()), overlaySegs, value))
          }
        val updated = DecodedPayload.writeDecodedFields(parent, mergedDecoded, mergedOverlay)
        payloads.update(basePath, updated)
        bumpRefreshFreshnessAt(basePath, segments, overlayPanel = false)
        if (overlay.isDefined || overlayUpdates.nonEmpty)
          bumpRefreshFreshnessAt(basePath, segments, overlayPanel = true)
        overlayUpdates.foreach { case (overlaySegs, _) =>
          bumpRefreshFreshnessAt(basePath, overlaySegs, overlayPanel = true)
        }
        watchOverlaySegments.getOrElse(watchedPath, Nil).foreach { overlaySegs =>
          bumpRefreshFreshnessAt(basePath, overlaySegs, overlayPanel = true)
        }
        if (detailViewPath.contains(basePath)) patchJsonViews(basePath, segments)
      }
    }
  }

  protected def syncWatchesFromJsonList(watches: List[Json]): Unit = {
    activeWatches.clear()
    watchOverlaySegments.clear()
    watches.foreach { w =>
      val c = w.hcursor
      (c.get[String]("path").toOption, c.get[Int]("watchId").toOption) match {
        case (Some(path), Some(watchId)) =>
          activeWatches.update(path, watchId)
          val overlaySegs =
            c.downField("overlaySegments").as[List[List[String]]].getOrElse(Nil)
          if (overlaySegs.nonEmpty) watchOverlaySegments.update(path, overlaySegs)
        case _ =>
          ()
      }
    }
    detailViewPath.foreach(p => payloads.get(p).foreach(showDetail(p, _)))
  }

}
