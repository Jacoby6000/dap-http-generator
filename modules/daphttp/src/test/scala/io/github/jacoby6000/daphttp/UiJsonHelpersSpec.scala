package io.github.jacoby6000.daphttp

import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

final class JsonPathSpec extends AnyFunSuite {
  test("get walks objects and array indices") {
    val json = Json.obj(
      "a" -> Json.arr(Json.obj("b" -> Json.fromInt(3)))
    )
    assert(JsonPath.get(json, List("a", "0", "b")).contains(Json.fromInt(3)))
    assert(JsonPath.get(json, List("missing")).isEmpty)
  }

  test("replace updates nested values and leaves siblings") {
    val json = Json.obj(
      "a" -> Json.fromInt(1),
      "b" -> Json.obj("c" -> Json.fromInt(2))
    )
    val updated = JsonPath.replace(json, List("b", "c"), Json.fromInt(9))
    assert(JsonPath.get(updated, List("a")).contains(Json.fromInt(1)))
    assert(JsonPath.get(updated, List("b", "c")).contains(Json.fromInt(9)))
  }
}

final class DecodedPayloadSpec extends AnyFunSuite {
  test("extractDecoded prefers reads[0].decoded then decoded") {
    val nested = Json.obj(
      "reads" -> Json.arr(Json.obj("decoded" -> Json.fromString("via-reads")))
    )
    val flat = Json.obj("decoded" -> Json.fromString("flat"))
    assert(DecodedPayload.extractDecoded(nested) == Json.fromString("via-reads"))
    assert(DecodedPayload.extractDecoded(flat) == Json.fromString("flat"))
  }

  test("writeDecodedFields updates flat and reads envelopes") {
    val flat = Json.obj("decoded" -> Json.fromInt(1))
    val written = DecodedPayload.writeDecodedFields(flat, Json.fromInt(2), Some(Json.fromInt(3)))
    assert(written.hcursor.get[Int]("decoded").contains(2))
    assert(written.hcursor.get[Int]("overlayDecoded").contains(3))
  }

  test("decodeFailed when error or null decoded") {
    assert(DecodedPayload.decodeFailed(Json.obj("error" -> Json.fromString("boom"))))
    assert(DecodedPayload.decodeFailed(Json.obj("decoded" -> Json.Null)))
    assert(!DecodedPayload.decodeFailed(Json.obj("decoded" -> Json.fromInt(1))))
  }

  test("decodeFailed and extractDecoded honor reads[0] errors") {
    val failedRead = Json.obj(
      "reads" -> Json.arr(
        Json.obj("path" -> Json.fromString("/api/x"), "error" -> Json.fromString("dap down"))
      )
    )
    assert(DecodedPayload.decodeFailed(failedRead))
    assert(DecodedPayload.decodeErrorMessage(failedRead) == "dap down")
    assert(DecodedPayload.extractDecoded(failedRead).isNull)
  }

  test("extractDecoded does not fall back to the envelope when reads is present") {
    val envelope = Json.obj(
      "route" -> Json.fromString("/api/x"),
      "reads" -> Json.arr(Json.obj("path" -> Json.fromString("/api/x")))
    )
    assert(DecodedPayload.extractDecoded(envelope).isNull)
    assert(DecodedPayload.decodeFailed(envelope))
  }

  test("extractOverlayDecoded does not fall back past a reads envelope") {
    val envelope = Json.obj(
      "overlayDecoded" -> Json.fromString("top"),
      "reads" -> Json.arr(Json.obj("path" -> Json.fromString("/api/x")))
    )
    assert(DecodedPayload.extractOverlayDecoded(envelope).isEmpty)
    assert(
      DecodedPayload
        .extractOverlayDecoded(
          Json.obj("overlayDecoded" -> Json.fromString("flat"))
        )
        .contains(Json.fromString("flat"))
    )
  }
}

final class FetchableRoutePathSpec extends AnyFunSuite {
  test("httpPathForField rejects metadata segments") {
    assert(
      FetchableRoutePath
        .httpPathForField("/api/r", List("_address"), Set("/api/r/_address"))
        .isEmpty
    )
  }

  test("httpPathForField accepts catalog paths and {index} templates") {
    val fetchable = Set("/api/arr/{index}", "/api/root", "/api/root/0")
    assert(
      FetchableRoutePath
        .httpPathForField("/api/arr/0", Nil, fetchable)
        .contains("/api/arr/0")
    )
    assert(
      FetchableRoutePath
        .httpPathForField("/api/root", List("child"), Set("/api/root", "/api/root/child"))
        .contains("/api/root/child")
    )
    assert(
      FetchableRoutePath
        .httpPathForField("/api/root", List("0", "nested"), fetchable)
        .contains("/api/root/0/nested")
    )
  }
}
