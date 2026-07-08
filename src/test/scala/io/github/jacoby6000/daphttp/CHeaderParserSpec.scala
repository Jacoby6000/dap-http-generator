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

    assert(structs.map(_._1) == List("Vec3f", "PlayerState"))
    assert(
      CHeaderParser
        .extractFields(structs.head._2)
        .map { case (_, declarator) => CHeaderParser.fieldName(declarator) } == List("x", "y", "z")
    )

    val playerStateFields = CHeaderParser.extractFields(structs(1)._2)
    assert(
      playerStateFields
        .find { case (_, declarator) => CHeaderParser.fieldName(declarator) == "buffer" }
        .exists { case (_, declarator) => CHeaderParser.pointerDepth(declarator) == 1 }
    )
    assert(
      playerStateFields
        .find { case (_, declarator) => CHeaderParser.fieldName(declarator) == "table" }
        .exists { case (_, declarator) => CHeaderParser.pointerDepth(declarator) == 2 }
    )
    assert(
      playerStateFields
        .find { case (_, declarator) => CHeaderParser.fieldName(declarator) == "history" }
        .flatMap { case (_, declarator) => CHeaderParser.arrayLength(declarator) }
        .contains(4)
    )
    assert(
      playerStateFields
        .find { case (_, declarator) => CHeaderParser.fieldName(declarator) == "score" }
        .exists { case (fieldType, _) => fieldType == "u128" }
    )
  }

  test("parses doldecomp fixture structs") {
    val source = scala.io.Source
      .fromFile("src/test/resources/doldecomp-fixture/include/player_state.h")
      .mkString
    val structs = CHeaderParser.parse(source)
    val playerState = structs.find(_._1 == "PlayerState").get

    assert(
      CHeaderParser
        .extractFields(playerState._2)
        .map { case (fieldType, declarator) =>
          CHeaderParser.fieldName(declarator) -> fieldType
        } == List(
        "health" -> "u32",
        "score" -> "u128",
        "position" -> "Vec3f"
      )
    )
  }

  test("parses global variable declarations from source files") {
    val source =
      """
        |typedef struct GameScene {
        |    u8 idx;
        |    u8 preload;
        |    u16 flags;
        |} GameScene;
        |
        |GameScene gm_803DDAC0_Scenes[] = { { 0 } };
        |static u8 gm_804D68C0;
        |""".stripMargin

    val declarations = CHeaderParser.parseGlobalDeclarations(source)

    assert(declarations.map(_.name) == List("gm_803DDAC0_Scenes", "gm_804D68C0"))
    assert(declarations.head.typeName == "GameScene")
    assert(declarations.head.isArray)
    assert(declarations.head.arrayLength.isEmpty)
    assert(!declarations(1).isArray)
  }

}
