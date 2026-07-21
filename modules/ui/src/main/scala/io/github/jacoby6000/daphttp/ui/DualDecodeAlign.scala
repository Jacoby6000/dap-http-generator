package io.github.jacoby6000.daphttp.ui

import io.circe.Json
import io.circe.JsonObject

/** One dual-tree row: source and overlay may use different field names (renames). */
final case class DualChild(
    sourceName: Option[String],
    overlayName: Option[String],
    source: Option[Json],
    overlay: Option[Json]
)

object DualDecodeAlign {

  def isMetaKey(name: String): Boolean = name.startsWith("_")

  def parseOffsets(obj: JsonObject): Map[String, Int] =
    obj("_offsets")
      .flatMap(_.asObject)
      .map { offs =>
        offs.toMap.flatMap { case (name, value) =>
          value.asNumber.flatMap(_.toInt).map(name -> _)
        }
      }
      .getOrElse(Map.empty)

  /** Align object fields by `_offsets` when present; otherwise by name (overlay-first). */
  def alignObjects(
      source: Option[JsonObject],
      overlay: Option[JsonObject]
  ): List[DualChild] = {
    val srcOff = source.map(parseOffsets).getOrElse(Map.empty)
    val ovOff = overlay.map(parseOffsets).getOrElse(Map.empty)

    val addressRow =
      (source.flatMap(_("_address")), overlay.flatMap(_("_address"))) match {
        case (None, None) => Nil
        case (s, o)       =>
          List(
            DualChild(
              sourceName = s.map(_ => "_address"),
              overlayName = o.map(_ => "_address"),
              source = s,
              overlay = o
            )
          )
      }

    val fieldRows =
      if (srcOff.nonEmpty || ovOff.nonEmpty)
        mergeByOffset(sideEntries(source, srcOff), sideEntries(overlay, ovOff))
      else
        mergeByName(source, overlay)

    addressRow ++ fieldRows
  }

  def alignArrays(
      source: List[Json],
      overlay: List[Json]
  ): List[DualChild] = {
    val n = math.max(source.size, overlay.size)
    (0 until n).map { i =>
      DualChild(
        sourceName = source.lift(i).map(_ => i.toString),
        overlayName = overlay.lift(i).map(_ => i.toString),
        source = source.lift(i),
        overlay = overlay.lift(i)
      )
    }.toList
  }

  /** (offset, declIndex, name, json) sorted by offset then declaration order. */
  private def sideEntries(
      obj: Option[JsonObject],
      offsets: Map[String, Int]
  ): List[(Int, Int, String, Json)] =
    obj match {
      case None    => Nil
      case Some(o) =>
        o.keys.toList.zipWithIndex
          .filter { case (name, _) => !isMetaKey(name) }
          .flatMap { case (name, idx) =>
            o(name).map { json =>
              val off = offsets.getOrElse(name, Int.MaxValue)
              (off, idx, name, json)
            }
          }
          .sortBy(e => (e._1, e._2))
    }

  private def mergeByOffset(
      source: List[(Int, Int, String, Json)],
      overlay: List[(Int, Int, String, Json)]
  ): List[DualChild] = {
    val srcGroups = source.groupBy(_._1).toList.sortBy(_._1)
    val ovGroups = overlay.groupBy(_._1).toList.sortBy(_._1)
    val out = scala.collection.mutable.ListBuffer.empty[DualChild]
    var i = 0
    var j = 0
    while (i < srcGroups.size || j < ovGroups.size) {
      val srcOff = if (i < srcGroups.size) Some(srcGroups(i)._1) else None
      val ovOff = if (j < ovGroups.size) Some(ovGroups(j)._1) else None
      (srcOff, ovOff) match {
        case (Some(so), Some(oo)) if so == oo =>
          val sList = srcGroups(i)._2
          val oList = ovGroups(j)._2
          val n = math.max(sList.size, oList.size)
          var k = 0
          while (k < n) {
            val s = sList.lift(k)
            val o = oList.lift(k)
            out += DualChild(
              sourceName = s.map(_._3),
              overlayName = o.map(_._3),
              source = s.map(_._4),
              overlay = o.map(_._4)
            )
            k += 1
          }
          i += 1
          j += 1
        case (Some(so), Some(oo)) if so < oo =>
          srcGroups(i)._2.foreach { s =>
            out += DualChild(Some(s._3), None, Some(s._4), None)
          }
          i += 1
        case (Some(_), Some(_)) =>
          ovGroups(j)._2.foreach { o =>
            out += DualChild(None, Some(o._3), None, Some(o._4))
          }
          j += 1
        case (Some(_), None) =>
          srcGroups(i)._2.foreach { s =>
            out += DualChild(Some(s._3), None, Some(s._4), None)
          }
          i += 1
        case (None, Some(_)) =>
          ovGroups(j)._2.foreach { o =>
            out += DualChild(None, Some(o._3), None, Some(o._4))
          }
          j += 1
        case (None, None) =>
          i = srcGroups.size
          j = ovGroups.size
      }
    }
    out.toList
  }

  private def mergeByName(
      source: Option[JsonObject],
      overlay: Option[JsonObject]
  ): List[DualChild] = {
    val srcMap = source.map(_.toMap).getOrElse(Map.empty[String, Json])
    val ovMap = overlay.map(_.toMap).getOrElse(Map.empty[String, Json])
    val keys =
      if (overlay.isDefined)
        overlay.get.keys.toList.filterNot(isMetaKey) ++
          srcMap.keys.toList.filter(k => !isMetaKey(k) && !ovMap.contains(k))
      else
        source.map(_.keys.toList.filterNot(isMetaKey)).getOrElse(Nil)
    keys.map { name =>
      DualChild(
        sourceName = srcMap.get(name).map(_ => name),
        overlayName = ovMap.get(name).map(_ => name),
        source = srcMap.get(name),
        overlay = ovMap.get(name)
      )
    }
  }
}
