package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite

final class IndexPathSpec extends AnyFunSuite {
  test("countSlots counts {index} segments") {
    assert(IndexPath.countSlots("/api/foo/{index}/bar/{index}") == 2)
    assert(IndexPath.countSlots("/api/foo") == 0)
  }

  test("substitute fills slots left to right") {
    assert(IndexPath.substitute("/api/a/{index}/b/{index}", List(3, 7)) == "/api/a/3/b/7")
    assert(IndexPath.substitute("/api/a/{index}", Nil) == "/api/a/0")
  }

  test("extractIndices matches templates to concrete paths") {
    assert(
      IndexPath
        .extractIndices("/api/slots/{index}/x", "/api/slots/2/x")
        .contains(List(2))
    )
    assert(IndexPath.extractIndices("/api/slots/{index}", "/api/other/1").isEmpty)
    assert(IndexPath.extractIndices("/api/slots/{index}", "/api/slots/x").isEmpty)
    assert(
      IndexPath
        .extractIndices("/api/slots/{index}", "/api/slots/99999999999999999999")
        .isEmpty
    )
  }

  test("resolveBrowse keeps current values for the same template") {
    val path = "/api/arr/{index}"
    assert(
      IndexPath
        .resolveBrowse(path, List(path), Some(path), List(4))
        .contains((path, List(4)))
    )
    assert(
      IndexPath
        .resolveBrowse(path, List(path), None, List(4))
        .contains((path, List(0)))
    )
  }

  test("resolveBrowse recovers template from concrete indexed path") {
    val template = "/api/arr/{index}/field"
    assert(
      IndexPath
        .resolveBrowse("/api/arr/9/field", List(template), None, Nil)
        .contains((template, List(9)))
    )
  }

  test("concretePath substitutes only when template matches selection") {
    val template = "/api/arr/{index}"
    assert(IndexPath.concretePath(template, Some(template), List(5)) == "/api/arr/5")
    assert(IndexPath.concretePath(template, Some("/other/{index}"), List(5)) == template)
    assert(IndexPath.concretePath("/api/plain", Some(template), List(5)) == "/api/plain")
  }
}
