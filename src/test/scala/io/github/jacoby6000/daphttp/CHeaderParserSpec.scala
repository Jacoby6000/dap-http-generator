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
        .map(field => CHeaderParser.fieldName(field.declarator)) == List("x", "y", "z")
    )

    val playerStateFields = CHeaderParser.extractFields(structs(1)._2)
    assert(
      playerStateFields
        .find(field => CHeaderParser.fieldName(field.declarator) == "buffer")
        .exists(field => CHeaderParser.pointerDepth(field.declarator) == 1)
    )
    assert(
      playerStateFields
        .find(field => CHeaderParser.fieldName(field.declarator) == "table")
        .exists(field => CHeaderParser.pointerDepth(field.declarator) == 2)
    )
    assert(
      playerStateFields
        .find(field => CHeaderParser.fieldName(field.declarator) == "history")
        .flatMap(field => CHeaderParser.arrayLength(field.declarator))
        .contains(4)
    )
    assert(
      playerStateFields
        .find(field => CHeaderParser.fieldName(field.declarator) == "score")
        .exists(field => field.typeName == "u128")
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
        .map(field => CHeaderParser.fieldName(field.declarator) -> field.typeName) == List(
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

  test("parses enum definitions with explicit and implicit values") {
    val source =
      """
        |#define BASE 10
        |#define NESTED BASE
        |#define PAREN (10)
        |#define SHIFT (1 << 3)
        |typedef enum Color {
        |    RED,
        |    GREEN = 2,
        |    BLUE,
        |    CUSTOM = BASE + 5,
        |    FROM_NESTED = NESTED,
        |    FROM_PAREN = PAREN,
        |    FROM_SHIFT = SHIFT,
        |    CASTED = (int)7,
        |    NEG = -1,
        |    REF = GREEN
        |} Color;
        |
        |enum Mode {
        |    MODE_A = 0x10,
        |    MODE_B
        |};
        |""".stripMargin

    val parsed = CHeaderParser.parseEnums(source)
    val enums = parsed.enums

    assert(parsed.warnings.isEmpty)
    assert(enums.contains("Color"))
    assert(
      enums("Color").values == List(
        IrEnumValue("RED", 0),
        IrEnumValue("GREEN", 2),
        IrEnumValue("BLUE", 3),
        IrEnumValue("CUSTOM", 15),
        IrEnumValue("FROM_NESTED", 10),
        IrEnumValue("FROM_PAREN", 10),
        IrEnumValue("FROM_SHIFT", 8),
        IrEnumValue("CASTED", 7),
        IrEnumValue("NEG", -1),
        IrEnumValue("REF", 2)
      )
    )
    assert(enums.contains("Mode"))
    assert(
      enums("Mode").values == List(
        IrEnumValue("MODE_A", 0x10),
        IrEnumValue("MODE_B", 0x11)
      )
    )
  }

  test("warns when an explicit enumerator initializer cannot be evaluated") {
    val source =
      """
        |typedef enum Broken {
        |    OK = 1,
        |    BAD = sizeof(struct Missing)
        |} Broken;
        |""".stripMargin

    val parsed = CHeaderParser.parseEnums(source)
    assert(parsed.enums("Broken").values.exists(_.name == "BAD"))
    assert(parsed.warnings.exists(_.contains("Broken.BAD")))
    assert(parsed.warnings.exists(_.contains("Unable to evaluate enumerator initializer")))
  }

  test("extracts parenthesized and chained macros via CDT preprocessor") {
    val source =
      """
        |#define BASE 10
        |#define NESTED BASE
        |#define PAREN (10)
        |#define SHIFT (1 << 3)
        |""".stripMargin

    val macros = CHeaderParser.extractMacros(source)
    assert(macros.get("BASE").contains("10"))
    assert(macros.get("NESTED").contains("BASE"))
    assert(macros.get("PAREN").contains("(10)"))
    assert(macros.get("SHIFT").contains("(1 << 3)"))
  }

  test("resolves array lengths and bitfield widths from macros via CDT ValueFactory") {
    val source =
      """
        |#define LEN 4
        |#define WIDTH (1 + 2)
        |typedef struct Table {
        |    u8 entries[LEN];
        |    u8 flag : WIDTH;
        |} Table;
        |""".stripMargin

    val macros = CHeaderParser.extractMacros(source)
    val structs = CHeaderParser.parse(source, macros)
    val fields = CHeaderParser.extractFields(structs.head._2)
    val entries = fields.find(f => CHeaderParser.fieldName(f.declarator) == "entries").get
    val flag = fields.find(f => CHeaderParser.fieldName(f.declarator) == "flag").get
    assert(CHeaderParser.arrayLength(entries.declarator).contains(4))
    assert(flag.bitFieldWidth.contains(3))
  }

  test("evaluates enum initializers that depend on macros from ScannerInfo") {
    val source =
      """
        |typedef enum Color {
        |    CUSTOM = BASE + 5,
        |    P = PAREN,
        |    S = SHIFT
        |} Color;
        |""".stripMargin

    val parsed = CHeaderParser.parseEnums(
      source,
      Map("BASE" -> "10", "PAREN" -> "(10)", "SHIFT" -> "(1 << 3)")
    )
    assert(parsed.warnings.isEmpty)
    assert(
      parsed.enums("Color").values == List(
        IrEnumValue("CUSTOM", 15),
        IrEnumValue("P", 10),
        IrEnumValue("S", 8)
      )
    )
  }

  test("parses nested and anonymous in-struct enums") {
    val source =
      """
        |typedef struct GameState {
        |    enum Phase {
        |        PHASE_INIT = 0,
        |        PHASE_RUN = 1
        |    } phase;
        |    enum {
        |        FLAG_A = 1,
        |        FLAG_B = 2
        |    } flags;
        |} GameState;
        |""".stripMargin

    val parsed = CHeaderParser.parseEnums(source)
    assert(parsed.enums.contains("Phase"))
    assert(
      parsed.enums("Phase").values == List(
        IrEnumValue("PHASE_INIT", 0),
        IrEnumValue("PHASE_RUN", 1)
      )
    )
    assert(parsed.enums.contains("FlagsEnum"))
    assert(
      parsed.enums("FlagsEnum").values == List(
        IrEnumValue("FLAG_A", 1),
        IrEnumValue("FLAG_B", 2)
      )
    )

    val structs = CHeaderParser.parse(source)
    val fields = CHeaderParser.extractFields(structs.head._2)
    assert(
      fields.map(f => CHeaderParser.fieldName(f.declarator) -> f.typeName) == List(
        "phase" -> "Phase",
        "flags" -> "FlagsEnum"
      )
    )
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

  test("strips static qualifier from global declaration types") {
    val source =
      """
        |static u8 event_match_selection_index_to_event_match_id_mapping[] = {
        |    0x00, 0x11, 0x02,
        |};
        |""".stripMargin

    val declaration = CHeaderParser.parseGlobalDeclarations(source).head

    assert(declaration.typeName == "u8")
    assert(declaration.isArray)
    assert(declaration.initializerLength.contains(3))
    assert(declaration.pointerDepth == 0)
  }

  test("parses pointer global declarations") {
    val source =
      """
        |typedef struct EventInitDataLevelTbl {
        |    u8 kind;
        |} EventInitDataLevelTbl;
        |
        |static struct EventInitDataLevelTbl** event_init_data_level_table[2];
        |""".stripMargin

    val declaration = CHeaderParser.parseGlobalDeclarations(source).head

    assert(declaration.name == "event_init_data_level_table")
    assert(declaration.typeName == "EventInitDataLevelTbl")
    assert(declaration.isArray)
    assert(declaration.declaratorLength.contains(2))
    assert(declaration.pointerDepth == 2)
  }

  test("parses anonymous union members with shared union groups") {
    val source =
      """
        |typedef struct EventInitDataLevelTbl {
        |    u8 kind;
        |    union {
        |        int* extra_character_init_data;
        |        int coin_goal;
        |    };
        |} EventInitDataLevelTbl;
        |""".stripMargin

    val fields = CHeaderParser.extractFields(CHeaderParser.parse(source).head._2)
    val unionFields = fields.filter(_.unionGroup.nonEmpty)

    assert(unionFields.size == 2)
    assert(unionFields.map(_.unionGroup).distinct.size == 1)
  }

  test("extracts bitfield widths from struct members") {
    val source =
      """
        |typedef struct StartEventRules {
        |    u8 x0_0 : 1;
        |    u8 x0_1 : 1;
        |    s8 x3;
        |} StartEventRules;
        |""".stripMargin

    val fields = CHeaderParser.extractFields(CHeaderParser.parse(source).head._2)

    assert(fields.take(2).map(_.bitFieldWidth) == List(Some(1), Some(1)))
    assert(fields(2).bitFieldWidth.isEmpty)
  }
}
