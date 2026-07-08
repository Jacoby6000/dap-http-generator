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
    assert(declarations.head.declaratorLength.isEmpty)
    assert(declarations.head.initializerLength.contains(1))
    assert(declarations.head.resolvedArrayLength.contains(1))
    assert(!declarations(1).isArray)
  }

  test("infers global array length from C initializer entry count") {
    val source =
      """
        |typedef struct GameScene {
        |    u8 idx;
        |    u8 preload;
        |    u16 flags;
        |} GameScene;
        |
        |GameScene gm_803DDAC0_Scenes[] = {
        |    { 0x00, 0x03, 0 },
        |    { 0x01, 0x03, 0 },
        |};
        |""".stripMargin

    val declaration = CHeaderParser.parseGlobalDeclarations(source).head

    assert(declaration.isArray)
    assert(declaration.declaratorLength.isEmpty)
    assert(declaration.initializerLength.contains(2))
    assert(declaration.resolvedArrayLength.contains(2))
  }

  test("infers char array length from string literal initializer") {
    val source =
      """
        |const char strPlLoadCommonData[] = "pLoadCommonData";
        |""".stripMargin

    val declaration = CHeaderParser.parseGlobalDeclarations(source).head

    assert(declaration.isArray)
    assert(declaration.initializerLength.contains(16))
    assert(declaration.resolvedArrayLength.contains(16))
  }

  test("infers struct field array length from accompanying C initializer") {
    val header =
      """
        |typedef struct GameScene {
        |    u8 idx;
        |    u8 preload;
        |    u16 flags;
        |} GameScene;
        |
        |typedef struct SceneTable {
        |    u8 count;
        |    GameScene scenes[];
        |} SceneTable;
        |""".stripMargin

    val source =
      """
        |#include "scene_table.h"
        |
        |SceneTable gm_scene_table = {
        |    2,
        |    {
        |        { 0x00, 0x03, 0 },
        |        { 0x01, 0x03, 0 },
        |    },
        |};
        |""".stripMargin

    val structs = CHeaderParser.parse(header).toMap
    val lengths = CHeaderParser.parseStructFieldInitializerLengths(source, structs)

    assert(lengths(("SceneTable", "scenes")) == 2)
  }
}
