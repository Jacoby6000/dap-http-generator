package io.github.jacoby6000.daphttp

import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

class DualDecodeAlignSpec extends AnyFunSuite {
  test("isMetaKey treats underscore-prefixed keys as metadata") {
    assert(DualDecodeAlign.isMetaKey("_address"))
    assert(DualDecodeAlign.isMetaKey("_offsets"))
    assert(!DualDecodeAlign.isMetaKey("x"))
  }

  test("alignObjects by name when offsets are absent") {
    val source = Json
      .obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2), "_address" -> Json.fromString("0x1"))
      .asObject
    val overlay = Json.obj("b" -> Json.fromInt(20), "c" -> Json.fromInt(3)).asObject
    val rows = DualDecodeAlign.alignObjects(source, overlay)
    assert(rows.exists(_.sourceName.contains("_address")))
    val names = rows.collect {
      case DualChild(Some(s), Some(o), _, _) if s == o => s
      case DualChild(Some(s), None, _, _)              => s
      case DualChild(None, Some(o), _, _)              => o
    }
    assert(names.contains("b"))
    assert(names.contains("c"))
    assert(names.contains("a"))
  }

  test("alignObjects by offset pairs renames across views") {
    val source = Json
      .obj(
        "x0" -> Json.fromInt(1),
        "pad" -> Json.fromInt(0),
        "_offsets" -> Json.obj("x0" -> Json.fromInt(0), "pad" -> Json.fromInt(2))
      )
      .asObject
    val overlay = Json
      .obj(
        "wide" -> Json.fromInt(0x10000),
        "_offsets" -> Json.obj("wide" -> Json.fromInt(0))
      )
      .asObject
    val rows = DualDecodeAlign.alignObjects(source, overlay)
    val renamed = rows.find(r => r.sourceName.contains("x0") && r.overlayName.contains("wide"))
    assert(renamed.isDefined)
    assert(rows.exists(_.sourceName.contains("pad")))
  }

  test("alignArrays pads the shorter side") {
    val rows = DualDecodeAlign.alignArrays(
      List(Json.fromInt(1)),
      List(Json.fromInt(10), Json.fromInt(20))
    )
    assert(rows.size == 2)
    assert(rows.head.source.contains(Json.fromInt(1)))
    assert(rows.head.overlay.contains(Json.fromInt(10)))
    assert(rows(1).source.isEmpty)
    assert(rows(1).overlay.contains(Json.fromInt(20)))
  }
}
