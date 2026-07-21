package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite
import software.amazon.smithy.model.shapes.ShapeId

import java.nio.file.Files

class ArrayLengthEnrichmentSpec extends AnyFunSuite {
  private def id(name: String): ShapeId = ShapeId.from(s"example#$name")

  test("parses Melee-style +hex offset comments") {
    val source =
      """
        |typedef struct plAttackStats {
        |    /*   +0 */ u32 total;
        |    /*   +4 */ u32 by_attack_counts[StatsAttack_Count];
        |    /* +194 */ u32 thrown_item_count;
        |    /* +19C */ u32 specials_count;
        |} plAttackStats;
        |""".stripMargin
    val offsets = CHeaderOffsetParser.parse(source)
    assert(offsets(("plAttackStats", "total")) == 0)
    assert(offsets(("plAttackStats", "by_attack_counts")) == 4)
    assert(offsets(("plAttackStats", "thrown_item_count")) == 0x194)
    assert(offsets(("plAttackStats", "specials_count")) == 0x19c)
  }

  test("infers arrayLength from offset-comment gap when enumerator bound is unresolved") {
    val member = IrMember(
      id = id("plAttackStats$byAttackCounts"),
      name = "byAttackCounts",
      target = IrType.Primitive(IrPrimitive.U32),
      staticAddress = None,
      paddingRepeats = None,
      isPointer = false,
      isArray = true,
      arrayLength = None,
      endianOverride = None,
      primitiveOverride = Some(IrPrimitive.U32)
    )
    val source =
      """
        |typedef struct plAttackStats {
        |    /*   +0 */ u32 total;
        |    /*   +4 */ u32 by_attack_counts[StatsAttack_Count];
        |    /* +194 */ u32 thrown_item_count;
        |} plAttackStats;
        |""".stripMargin
    val fields = CHeaderParser.extractFields(CHeaderParser.parse(source).head._2)
    val comments = CHeaderOffsetParser.parse(source)
    val enriched = DoldecompIrGenerator.enrichMissingArrayLengths(
      structName = "plAttackStats",
      fields = fields,
      members = List(member),
      commentOffsets = comments,
      arrayConstants = Map.empty,
      wordSizeBits = 32
    )
    assert(enriched.head.arrayLength.contains(0x64)) // (0x194 - 4) / 4
    assert(enriched.head.target.isInstanceOf[IrType.ListType])
  }

  test("packs plAttackStats when StatsAttack_Count is missing but offset comments exist") {
    val tmp = Files.createTempDirectory("dap-attack-stats")
    try {
      val header = tmp.resolve("attack.h")
      val symbols = tmp.resolve("symbols.txt")
      Files.writeString(
        header,
        """
          |typedef struct plAttackStats {
          |    /*   +0 */ u32 total;
          |    /*   +4 */ u32 by_attack_counts[StatsAttack_Count];
          |    /* +194 */ u32 thrown_item_count;
          |    /* +198 */ u32 aerials_count;
          |    /* +19C */ u32 specials_count;
          |    /* +1A0 */ u32 x1A0_count;
          |    /* +1A4 */ u32 x1A4_count;
          |    /* +1A8 */ u32 x1A8;
          |} plAttackStats;
          |
          |plAttackStats gAttackStats;
          |""".stripMargin
      )
      Files.writeString(
        symbols,
        "gAttackStats = .data:0x80000000; // type:object size:0x1AC scope:global ctype:plAttackStats\n"
      )

      val generation = DoldecompIrGenerator
        .generateFromPaths(
          symbolsPath = symbols,
          headerRoots = List(tmp),
          namespace = "example.attack",
          serviceName = "Api",
          wordSizeBits = 32
        )
        .toOption
        .get

      val struct = generation.services.head.operations.head.output.members.head.target
        .asInstanceOf[IrType.MemoryMappedStruct]
      val counts = struct.members.find(_.name == "byAttackCounts").get
      assert(counts.isArray)
      assert(counts.arrayLength.contains(0x64))
      assert(counts.offsetBytes.contains(4))
      assert(struct.members.find(_.name == "thrownItemCount").get.offsetBytes.contains(0x194))
      assert(struct.declaredSizeBytes.contains(0x1ac))
    } finally {
      Files.walk(tmp).sorted(java.util.Comparator.reverseOrder()).forEach { path =>
        val _ = Files.deleteIfExists(path)
      }
    }
  }

  test("resolves arrayLength from enumerator when StatsAttack_Count is present") {
    val tmp = Files.createTempDirectory("dap-attack-stats-enum")
    try {
      val header = tmp.resolve("attack.h")
      val symbols = tmp.resolve("symbols.txt")
      Files.writeString(
        header,
        """
          |typedef enum StatsAttack {
          |    StatsAttack_A,
          |    StatsAttack_B,
          |    StatsAttack_Count
          |} StatsAttack;
          |
          |typedef struct plAttackStats {
          |    u32 total;
          |    u32 by_attack_counts[StatsAttack_Count];
          |    u32 thrown_item_count;
          |} plAttackStats;
          |
          |plAttackStats gAttackStats;
          |""".stripMargin
      )
      Files.writeString(
        symbols,
        "gAttackStats = .data:0x80000000; // type:object size:0x10 scope:global ctype:plAttackStats\n"
      )

      val generation = DoldecompIrGenerator
        .generateFromPaths(
          symbolsPath = symbols,
          headerRoots = List(tmp),
          namespace = "example.attackenum",
          serviceName = "Api",
          wordSizeBits = 32
        )
        .toOption
        .get

      val struct = generation.services.head.operations.head.output.members.head.target
        .asInstanceOf[IrType.MemoryMappedStruct]
      val counts = struct.members.find(_.name == "byAttackCounts").get
      assert(counts.arrayLength.contains(2))
      assert(counts.offsetBytes.contains(4))
    } finally {
      Files.walk(tmp).sorted(java.util.Comparator.reverseOrder()).forEach { path =>
        val _ = Files.deleteIfExists(path)
      }
    }
  }
}
