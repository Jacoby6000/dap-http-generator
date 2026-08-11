package io.github.jacoby6000.daphttp

import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

final class FieldFreshnessSpec extends AnyFunSuite {
  test("stampKey prefixes overlay panel keys") {
    assert(FieldFreshness.stampKey("/api/a", List("x"), overlayPanel = false) == "/api/a/x")
    assert(FieldFreshness.stampKey("/api/a", List("x"), overlayPanel = true) == "ov:/api/a/x")
  }

  test("ageVisual fades then mutes after one minute") {
    val (freshOp, freshTint) = FieldFreshness.ageVisual(0)
    assert(freshOp == 1.0 && freshTint == 0.0)
    val (midOp, midTint) = FieldFreshness.ageVisual(30_000)
    assert(midOp < 1.0 && midTint > 0.0)
    val (staleOp, staleTint) = FieldFreshness.ageVisual(120_000)
    assert(staleOp < midOp && staleTint > midTint)
  }

  test("resolveFreshMs walks ancestors") {
    val stamped = Map(List("a") -> 10.0)
    assert(
      FieldFreshness.resolveFreshMs(
        segs => stamped.get(segs),
        List("a", "b"),
        fallback = 99.0
      ) == 10.0
    )
    assert(
      FieldFreshness.resolveFreshMs(_ => None, List("a"), fallback = 99.0) == 99.0
    )
  }
}

final class JsonPrimitiveDisplaySpec extends AnyFunSuite {
  test("cssClass and text cover primitives") {
    assert(JsonPrimitiveDisplay.cssClass(Json.Null) == "jv-null")
    assert(JsonPrimitiveDisplay.cssClass(Json.fromBoolean(true)) == "jv-bool")
    assert(JsonPrimitiveDisplay.text(Json.fromString("hi")) == "\"hi\"")
  }
}
