package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files

class StatsAttackEnumSpec extends AnyFunSuite {
  private val enumBody =
    """
      |typedef enum {
      |    /* 00 */ StatsAttack_None,
      |    /* 01 */ StatsAttack_Attack11,
      |    /* 02 */ StatsAttack_Attack12,
      |    /* 63 */ StatsAttack_99,
      |    /* 64 */ StatsAttack_Count,
      |} plStats_Attack;
      |""".stripMargin

  // Sibling enum with an unevaluable initializer forces loadEnums to inject Count macros and
  // reparse — the case that used to drop plStats_Attack from the final pass alone.
  private val warningSiblingEnum =
    """
      |typedef enum {
      |    Warning_A = MISSING_MACRO,
      |    Warning_Count,
      |} WarningEnum;
      |""".stripMargin

  test("anonymous typedef enum harvests StatsAttack_Count") {
    val parsed = CHeaderParser.parseEnums(enumBody)
    assert(parsed.enums.contains("plStats_Attack"))
    val count = parsed.enums("plStats_Attack").values.find(_.name == "StatsAttack_Count")
    assert(count.isDefined, s"values=${parsed.enums("plStats_Attack").values.map(_.name)}")
    assert(
      count.get.value == 4
    ) // None, Attack11, Attack12, 99, Count → Count=4 in this shortened fixture
  }

  test("Count macro reparse keeps StatsAttack_Count via multi-pass merge") {
    // CDT expands StatsAttack_Count away in the defining TU once it is a ScannerInfo macro; the
    // generator must still retain the pre-injection enum body for array-bound lookup.
    val tmp = Files.createTempDirectory("dap-stats-attack-reparse")
    try {
      val enumFile = tmp.resolve("plstats.c")
      val warnFile = tmp.resolve("warning.c")
      val hdr = tmp.resolve("plattack.h")
      val symbols = tmp.resolve("symbols.txt")
      Files.writeString(enumFile, enumBody + "\n")
      Files.writeString(warnFile, warningSiblingEnum + "\n")
      Files.writeString(
        hdr,
        """
          |typedef struct plAttackStats {
          |    /*   +0 */ u32 total;
          |    /*   +4 */ u32 by_attack_counts[StatsAttack_Count];
          |    /* +194 */ u32 thrown_item_count;
          |} plAttackStats;
          |plAttackStats gAttackStats;
          |""".stripMargin
      )
      Files.writeString(
        symbols,
        "gAttackStats = .data:0x80000000; // type:object size:0x198 scope:global ctype:plAttackStats\n"
      )
      val generation = DoldecompIrGenerator
        .generateFromPaths(symbols, List(tmp), "example.stats", "Api", 32)
        .toOption
        .get
      val struct = generation.services.head.operations.head.output.members.head.target
        .asInstanceOf[IrType.MemoryMappedStruct]
      val counts = struct.members.find(_.name == "byAttackCounts").get
      // Comment gap would give (0x194-4)/4=0x64; enum path after multi-pass merge gives 4.
      assert(
        counts.arrayLength.contains(4),
        s"got ${counts.arrayLength}, warnings=${generation.warnings}"
      )
    } finally {
      Files
        .walk(tmp)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach { p =>
          val _ = Files.deleteIfExists(p)
        }
    }
  }

  test("full pipeline resolves by_attack_counts from plStats_Attack enum in .c file") {
    val tmp = Files.createTempDirectory("dap-stats-attack")
    try {
      val src = tmp.resolve("plattack.c")
      val hdr = tmp.resolve("plattack.h")
      val symbols = tmp.resolve("symbols.txt")
      Files.writeString(src, enumBody + "\n")
      Files.writeString(
        hdr,
        """
          |typedef struct plAttackStats {
          |    /*   +0 */ u32 total;
          |    /*   +4 */ u32 by_attack_counts[StatsAttack_Count];
          |    /* +194 */ u32 thrown_item_count;
          |} plAttackStats;
          |plAttackStats gAttackStats;
          |""".stripMargin
      )
      Files.writeString(
        symbols,
        "gAttackStats = .data:0x80000000; // type:object size:0x198 scope:global ctype:plAttackStats\n"
      )
      val generation = DoldecompIrGenerator
        .generateFromPaths(symbols, List(tmp), "example.stats", "Api", 32)
        .toOption
        .get
      val struct = generation.services.head.operations.head.output.members.head.target
        .asInstanceOf[IrType.MemoryMappedStruct]
      val counts = struct.members.find(_.name == "byAttackCounts").get
      // Short fixture Count=4; real Melee is 0x64 — here we assert enum path worked (not comment gap).
      // Comment gap would give (0x194-4)/4=0x64; enum path gives 4.
      assert(
        counts.arrayLength.contains(4),
        s"got ${counts.arrayLength}, warnings=${generation.warnings}"
      )
    } finally {
      Files
        .walk(tmp)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach { p =>
          val _ = Files.deleteIfExists(p)
        }
    }
  }
}
