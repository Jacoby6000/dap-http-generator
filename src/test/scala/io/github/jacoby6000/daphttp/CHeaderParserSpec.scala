package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite

class CHeaderParserSpec extends AnyFunSuite {
  test("parses typedef struct declarations") {
    val source =
      """
        |typedef struct Vec3f {
        |    f32 x;
        |    f32 y;
        |    f32 z;
        |} Vec3f;
        |
        |typedef struct PlayerState {
        |    u8* buffer;
        |    u8** table;
        |    u32 health;
        |    u128 score;
        |    f32 history[4];
        |    Vec3f position;
        |} PlayerState;
        |""".stripMargin

    val structs = CHeaderParser.parse(source)

    assert(structs.map(_.name) == List("Vec3f", "PlayerState"))
    assert(structs.head.fields.map(_.name) == List("x", "y", "z"))

    val playerStateFields = structs(1).fields
    assert(playerStateFields.find(_.name == "buffer").exists(_.pointerDepth == 1))
    assert(playerStateFields.find(_.name == "table").exists(_.pointerDepth == 2))
    assert(playerStateFields.find(_.name == "history").flatMap(_.arrayLength).contains(4))
    assert(playerStateFields.find(_.name == "score").exists(_.typeName == "u128"))
  }

  test("parses doldecomp fixture structs") {
    val source = scala.io.Source
      .fromFile("src/test/resources/doldecomp-fixture/include/player_state.h")
      .mkString
    val structs = CHeaderParser.parse(source)
    val playerState = structs.find(_.name == "PlayerState").get

    assert(
      playerState.fields.map(field => field.name -> field.typeName) == List(
        "health" -> "u32",
        "score" -> "u128",
        "position" -> "Vec3f"
      )
    )
  }

}
