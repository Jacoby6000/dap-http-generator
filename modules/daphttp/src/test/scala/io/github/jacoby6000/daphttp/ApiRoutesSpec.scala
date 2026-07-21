package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite

class ApiRoutesSpec extends AnyFunSuite {
  test("normalize prefixes logical paths under /api") {
    assert(ApiRoutes.normalize("/ComplexApi/gFighterInfo") == "/api/ComplexApi/gFighterInfo")
    assert(ApiRoutes.normalize("ComplexApi/gFighterInfo") == "/api/ComplexApi/gFighterInfo")
  }

  test("normalize is idempotent") {
    val once = ApiRoutes.normalize("/MeleeApi/gPlayerState")
    assert(ApiRoutes.normalize(once) == once)
    assert(once == "/api/MeleeApi/gPlayerState")
  }

  test("isDataPath recognizes /api routes only") {
    assert(ApiRoutes.isDataPath("/api/Foo/bar"))
    assert(!ApiRoutes.isDataPath("/health"))
    assert(!ApiRoutes.isDataPath("/routes"))
    assert(!ApiRoutes.isDataPath("/assets/main.js"))
  }
}
