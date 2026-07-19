package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite

class CHeaderOffsetParserSpec extends AnyFunSuite {
  test("parses doldecomp-style member offset comments") {
    val source =
      """
        |typedef struct Example {
        |    /* 0x00 */ u8 kind;
        |    /* 0x04 */ union {
        |        int* extra_character_init_data;
        |        int coin_goal;
        |    };
        |    /* 0x08 */ u32 rules;
        |} Example;
        |""".stripMargin

    val offsets = CHeaderOffsetParser.parse(source)

    assert(offsets(("Example", "kind")) == 0x00)
    assert(offsets(("Example", "extra_character_init_data")) == 0x04)
    assert(offsets(("Example", "coin_goal")) == 0x04)
    assert(offsets(("Example", "rules")) == 0x08)
  }

  test("parses offsets with gaps between members") {
    val source =
      """
        |typedef struct Padded {
        |    /* 0x00 */ u8 a;
        |    /* 0x08 */ u32 b;
        |} Padded;
        |""".stripMargin

    val offsets = CHeaderOffsetParser.parse(source)

    assert(offsets(("Padded", "a")) == 0x00)
    assert(offsets(("Padded", "b")) == 0x08)
  }

  test("anchors a co-declared field offset to the first declarator") {
    val source =
      """
        |typedef struct Grouped {
        |    /* 0x00 */ u8 first, second;
        |    /* 0x02 */ u8 third;
        |} Grouped;
        |""".stripMargin

    val offsets = CHeaderOffsetParser.parse(source)

    assert(offsets(("Grouped", "first")) == 0x00)
    assert(!offsets.contains(("Grouped", "second")))
    assert(offsets(("Grouped", "third")) == 0x02)
  }

  test("records nested named struct fields under the inner type tag") {
    val source =
      """
        |typedef struct Outer {
        |    /* 0x00 */ u32 a;
        |    struct Inner {
        |        /* 0x00 */ u32 x;
        |        /* 0x04 */ u32 y;
        |    } inner;
        |    /* 0x0C */ u32 b;
        |} Outer;
        |""".stripMargin

    val offsets = CHeaderOffsetParser.parse(source)

    assert(offsets(("Outer", "a")) == 0x00)
    assert(offsets(("Outer", "b")) == 0x0c)
    assert(offsets(("Inner", "x")) == 0x00)
    assert(offsets(("Inner", "y")) == 0x04)
    assert(!offsets.contains(("Outer", "x")))
  }
}
