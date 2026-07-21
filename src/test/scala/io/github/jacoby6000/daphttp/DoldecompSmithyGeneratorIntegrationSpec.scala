package io.github.jacoby6000.daphttp

import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.shapes.ShapeId

import java.nio.file.Paths

class DoldecompSmithyGeneratorIntegrationSpec extends AnyFunSuite {
  test("generates IR from symbols and headers then compiles route plans") {
    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture")
    val irServices = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot.resolve("include")),
        namespace = "example.doldecomp",
        serviceName = "MeleeApi",
        wordSizeBits = 32
      )
      .toOption
      .get
      .services

    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(irServices).routes
    val route = plans("/api/MeleeApi/gPlayerState")
    val playerState =
      irServices.head.operations.head.output.members.head.target.asInstanceOf[IrType.Struct]
    val scoreMember = playerState.members.find(_.name == "score").get

    assert(route.reads.size == 1)
    assert(route.reads.head.address == 0x80453100L)
    // u32 + pad + u128 + Vec3f, with PPC32 max-align 8 → sizeof 0x28
    assert(route.reads.head.sizeBytes == 40)
    assert(route.reads.head.decodeCodec.nonEmpty)
    assert(!scoreMember.isPointer)
    assert(scoreMember.target == IrType.Primitive(IrPrimitive.U128))
    assert(scoreMember.offsetBytes.contains(8))

    val payload = Array[Byte](
      // health @0
      0x00,
      0x00,
      0x00,
      0x2a,
      // pad to score @8
      0x00,
      0x00,
      0x00,
      0x00,
      // score u128 @8
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x05,
      // position @24
      0x3f,
      0x80.toByte,
      0x00,
      0x00,
      0x40,
      0x00,
      0x00,
      0x00,
      0x40,
      0x40,
      0x00,
      0x00,
      // trailing sizeof pad
      0x00,
      0x00,
      0x00,
      0x00
    )
    val decoded =
      route.reads.head.decodeCodec.get.decode(scodec.bits.BitVector(payload)).toOption.get.value
    val cursor = decoded.hcursor
    assert(cursor.downField("health").as[Long].toOption.contains(42L))
    assert(cursor.downField("score").as[String].toOption.contains("5"))
    assert(cursor.downField("position").downField("x").as[Double].toOption.contains(1.0d))
    assert(cursor.downField("position").downField("y").as[Double].toOption.contains(2.0d))
    assert(cursor.downField("position").downField("z").as[Double].toOption.contains(3.0d))
  }

  test("generates IR from multiple headers with includes and compiles all routes") {
    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture-multi-header")
    val irServices = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot.resolve("include")),
        namespace = "example.doldecomp.multi",
        serviceName = "MeleeApi",
        wordSizeBits = 32
      )
      .toOption
      .get
      .services

    val service = irServices.head
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(irServices).routes
    val playerRoute = plans("/api/MeleeApi/gPlayerState")
    val worldRoute = plans("/api/MeleeApi/gWorldState")

    assert(service.operations.map(_.name).toSet == Set("GetGPlayerState", "GetGWorldState"))
    assert(playerRoute.reads.head.address == 0x80453100L)
    assert(worldRoute.reads.head.address == 0x80453200L)
    assert(playerRoute.reads.head.sizeBytes == 32)
    assert(worldRoute.reads.head.sizeBytes == 36)
    assert(playerRoute.reads.head.decodeCodec.nonEmpty)
    assert(worldRoute.reads.head.decodeCodec.nonEmpty)

    val playerState = service.operations
      .find(_.name == "GetGPlayerState")
      .get
      .output
      .members
      .head
      .target
      .asInstanceOf[IrType.Struct]
    val inventoryMember = playerState.members.find(_.name == "inventory").get
    val scratchMember = playerState.members.find(_.name == "scratch").get

    assert(inventoryMember.isArray)
    assert(inventoryMember.arrayLength.contains(2))
    assert(
      inventoryMember.target
        .asInstanceOf[IrType.ListType]
        .element
        .asInstanceOf[IrType.Struct]
        .members
        .exists(_.name == "quantity")
    )
    assert(scratchMember.isPointer)
    assert(scratchMember.target == IrType.Primitive(IrPrimitive.LongWord))
  }

  test("generates IR from C declarations without ctype metadata") {
    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture-declarations")
    val irServices = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot),
        namespace = "example.doldecomp.declarations",
        serviceName = "MeleeApi",
        wordSizeBits = 32
      )
      .toOption
      .get
      .services

    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(irServices).routes
    val route = plans("/api/MeleeApi/gPlayerState")

    assert(route.reads.size == 1)
    assert(route.reads.head.address == 0x80453100L)
    assert(route.reads.head.sizeBytes == 32)
    assert(route.reads.head.decodeCodec.nonEmpty)
  }

  test("generates IR for melee-style data arrays declared in C source") {
    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture-melee-style")
    val irServices = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot),
        namespace = "example.doldecomp.melee",
        serviceName = "MeleeApi",
        wordSizeBits = 32
      )
      .toOption
      .get
      .services

    val operation = irServices.head.operations.head
    val valueMember = operation.output.members.head

    assert(operation.name == "GetGm803DDAC0Scenes")
    // ctype:GameScene is present in symbols.txt; array metadata must still come from the C decl.
    assert(valueMember.isArray)
    assert(valueMember.arrayLength.contains(2))

    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(irServices).routes
    val route = plans("/api/MeleeApi/gm_803DDAC0_Scenes")

    assert(route.reads.head.address == 0x803ddac0L)
    // GameScene packs to 24 bytes (fnptrs + nested GameSceneInfo); array length 2 → 48
    assert(route.reads.head.sizeBytes == 48)
    assert(route.reads.head.decodeCodec.nonEmpty)
  }

  test("preserves symbol readSizeBytes through Smithy emit/load") {
    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture-offsets")
    val original = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot),
        namespace = "example.doldecomp.offsets",
        serviceName = "DolDecompApi",
        wordSizeBits = 32
      )
      .toOption
      .get
      .services

    val smithyText =
      SmithyIrEmitter.emit(original).fold(errors => fail(errors.mkString("\n")), identity)
    val model = Model
      .assembler()
      .addImport("src/main/smithy/dap-http-traits.smithy")
      .addUnparsedModel("offsets.smithy", smithyText)
      .assemble()
      .unwrap()
    val roundTripped = SmithyIrGenerator
      .generateFromModel(model)
      .fold(errors => fail(errors.mkString("\n")), identity)

    assert(original.head.operations.head.output.members.head.readSizeBytes.contains(0x08))
    assert(roundTripped.head.operations.head.output.members.head.readSizeBytes.contains(0x08))

    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(roundTripped)
    assert(plans.routes("/api/DolDecompApi/padded_struct").reads.head.sizeBytes == 0x08)
  }

  test("returns successful routes when some symbol derivations fail") {
    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture-partial")
    val generation = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot),
        namespace = "example.doldecomp.partial",
        serviceName = "MeleeApi",
        wordSizeBits = 32
      )
      .toOption
      .get

    assert(generation.warnings.exists(_.contains("badSymbol")))
    assert(
      generation.warnings.exists(w =>
        w.contains("orphanSymbol") && w.contains("no matching global C declaration")
      )
    )
    assert(
      generation.warnings.exists(w => w.contains("known code section") && w.contains(".text"))
    )
    assert(generation.services.head.operations.map(_.name) == List("GetGPlayerState"))

    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generation.services)
    assert(plans.routes.contains("/api/MeleeApi/gPlayerState"))
    assert(!plans.routes.contains("/api/MeleeApi/badSymbol"))
    assert(!plans.routes.contains("/api/MeleeApi/orphanSymbol"))
    assert(!plans.routes.contains("/api/MeleeApi/textObject"))
    assert(plans.errors.isEmpty)
  }

  test("generates IR for primitive globals without treating them as structs") {
    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture-primitive")
    val generation = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot),
        namespace = "example.doldecomp.primitive",
        serviceName = "MeleeApi",
        wordSizeBits = 32
      )
      .toOption
      .get

    assert(generation.warnings.isEmpty)
    assert(generation.services.head.operations.map(_.name) == List("GetIntVar"))

    val valueMember = generation.services.head.operations.head.output.members.head
    assert(valueMember.target == IrType.Primitive(IrPrimitive.S32))

    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generation.services)
    assert(plans.routes.contains("/api/MeleeApi/intVar"))
    assert(plans.routes("/api/MeleeApi/intVar").reads.head.sizeBytes == 4)
  }

  test("maps C enums to intEnum IR and named JSON decode") {
    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture-enums")
    val generation = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot),
        namespace = "example.doldecomp.enums",
        serviceName = "EnumApi",
        wordSizeBits = 32
      )
      .toOption
      .get

    assert(generation.warnings.isEmpty)
    val operations = generation.services.head.operations.map(op => op.name -> op).toMap

    val modeMember = operations("GetGCurrentMode").output.members.head
    val modeEnum = modeMember.target.asInstanceOf[IrType.IntEnum]
    assert(
      modeEnum.values
        .map(v => v.name -> v.value) == List("MODE_MENU" -> 0, "MODE_VS" -> 1, "MODE_STORY" -> 2)
    )

    val stateStruct = operations("GetGGameState").output.members.head.target
      .asInstanceOf[IrType.MemoryMappedStruct]
    val nestedMode =
      stateStruct.members.find(_.name == "mode").get.target.asInstanceOf[IrType.IntEnum]
    assert(nestedMode.values.map(_.name) == List("MODE_MENU", "MODE_VS", "MODE_STORY"))

    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generation.services)
    assert(plans.errors.isEmpty)
    val modeRead = plans.routes("/api/EnumApi/gCurrentMode").reads.head
    val decoded = modeRead.decodeCodec.get
      .decode(scodec.bits.BitVector(Array[Byte](0x00, 0x00, 0x00, 0x01)))
      .toOption
      .get
      .value
    assert(decoded == io.circe.Json.fromString("MODE_VS"))
    val unknown = modeRead.decodeCodec.get
      .decode(scodec.bits.BitVector(Array[Byte](0x00, 0x00, 0x00, 0x2a)))
      .toOption
      .get
      .value
    assert(unknown == io.circe.Json.fromString("0x2a"))
  }

  test("warns on conflicting multi-file enum definitions and keeps the first") {
    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture-enum-merge")
    val generation = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot),
        namespace = "example.doldecomp.enummerge",
        serviceName = "EnumMergeApi",
        wordSizeBits = 32
      )
      .toOption
      .get

    assert(generation.warnings.exists(_.contains("Conflicting enum definitions")))
    assert(generation.warnings.exists(_.contains("Color")))
    val color = generation.services.head.operations.head.output.members.head.target
      .asInstanceOf[IrType.MemoryMappedStruct]
      .members
      .find(_.name == "color")
      .get
      .target
      .asInstanceOf[IrType.IntEnum]
    assert(
      color.values.map(_.name) == List("COLOR_RED", "COLOR_BLUE") ||
        color.values.map(_.name) == List("COLOR_RED", "COLOR_GREEN")
    )
  }

  test("resolves cross-file enumerator initializers via accumulated macros") {
    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture-enum-crossref")
    val generation = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot),
        namespace = "example.doldecomp.enumcross",
        serviceName = "EnumCrossApi",
        wordSizeBits = 32
      )
      .toOption
      .get

    assert(
      !generation.warnings.exists(_.contains("Unable to evaluate enumerator initializer")),
      generation.warnings.mkString("\n")
    )
    val holder = generation.services.head.operations.head.output.members.head.target
      .asInstanceOf[IrType.MemoryMappedStruct]
    val stateEnum = holder.members.find(_.name == "state").get.target.asInstanceOf[IrType.IntEnum]
    assert(
      stateEnum.values == List(
        IrEnumValue("ftCh_MS_Count", 2),
        IrEnumValue("ftCh_MS_SelfCount", 0)
      )
    )
  }

  test("sets primitiveOverride for int typedef members like enum_t") {
    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture-typedef-int")
    val generation = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot),
        namespace = "example.doldecomp.typedefint",
        serviceName = "TypedefIntApi",
        wordSizeBits = 32
      )
      .toOption
      .get

    val holder = generation.services.head.operations.head.output.members.head.target
      .asInstanceOf[IrType.MemoryMappedStruct]
    val kind = holder.members.find(_.name == "kind").get
    val bufferId = holder.members.find(_.name == "bufferId").get
    val rawInt = holder.members.find(_.name == "rawInt").get
    assert(kind.primitiveOverride.contains(IrPrimitive.S32))
    assert(bufferId.primitiveOverride.contains(IrPrimitive.S32))
    assert(rawInt.primitiveOverride.contains(IrPrimitive.S32))
    assert(IrSizingWarnings.collect(generation.services).isEmpty)
  }

  test("resolves UNK_T macro types to pointer-sized primitives") {
    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture-unk-macro")
    val generation = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot),
        namespace = "example.doldecomp.unkmacro",
        serviceName = "UnkMacroApi",
        wordSizeBits = 32
      )
      .toOption
      .get

    assert(
      !generation.warnings.exists(_.contains("Missing struct or primitive definition")),
      generation.warnings.mkString("\n")
    )
    assert(generation.services.head.operations.map(_.name) == List("GetGOpaque"))
    val member = generation.services.head.operations.head.output.members.head
    // UNK_T expands to void* → word-sized opaque primitive (pointer depth may be on the declarator).
    assert(member.target == IrType.Primitive(IrPrimitive.LongWord) || member.isPointer)
  }

  test("maps char arrays and char pointers to string semantics in IR") {
    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture-strings")
    val generation = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot),
        namespace = "example.doldecomp.strings",
        serviceName = "StringsApi",
        wordSizeBits = 32
      )
      .toOption
      .get

    assert(generation.warnings.isEmpty)
    val struct =
      generation.services.head.operations.head.output.members.head.target
        .asInstanceOf[IrType.MemoryMappedStruct]
    val nameMember = struct.members.find(_.name == "name").get
    val labelMember = struct.members.find(_.name == "label").get

    assert(nameMember.isArray)
    assert(nameMember.arrayLength.contains(8))
    assert(nameMember.primitiveOverride.contains(IrPrimitive.Char))
    assert(labelMember.isPointer)
    assert(labelMember.primitiveOverride.contains(IrPrimitive.Char))

    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generation.services)
    assert(plans.routes.contains("/api/StringsApi/g_string_fields"))
    assert(plans.routes("/api/StringsApi/g_string_fields").reads.head.decodeCodec.nonEmpty)
  }

  test("decodes char globals as full null-terminated strings") {
    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture-char-string")
    val generation = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot),
        namespace = "example.doldecomp.charstring",
        serviceName = "DolDecompApi",
        wordSizeBits = 32
      )
      .toOption
      .get

    assert(generation.warnings.isEmpty)
    val route =
      HttpRouteIrEmitter
        .emitRoutePlansFromIr(generation.services)
        .routes(
          "/api/DolDecompApi/strPlLoadCommonData"
        )
    val payload = "pLoadCommonData\u0000\u0000".getBytes("US-ASCII")
    val decoded =
      route.reads.head.decodeCodec.get.decode(scodec.bits.BitVector(payload)).toOption.get.value

    assert(route.reads.head.sizeBytes == 17)
    assert(decoded == Json.fromString("pLoadCommonData"))
  }

  test("generates routes for static u8 arrays and pointer table globals") {
    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture-pointer-chain")
    val generation = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot),
        namespace = "example.doldecomp.pointer",
        serviceName = "MeleeApi",
        wordSizeBits = 32
      )
      .toOption
      .get

    assert(
      !generation.warnings.exists(
        _.contains("Missing struct or primitive definition for resolved type 'static u8'")
      )
    )
    assert(
      generation.services.head.operations.map(_.name).toSet == Set(
        "GetEventInitDataLevelTable",
        "GetEventMatchSelectionIndexToEventMatchIdMapping"
      )
    )

    val mappingMember =
      generation.services.head.operations
        .find(_.name == "GetEventMatchSelectionIndexToEventMatchIdMapping")
        .get
        .output
        .members
        .head
    assert(mappingMember.isArray)
    assert(mappingMember.arrayLength.contains(5))
    assert(
      mappingMember.target.asInstanceOf[IrType.ListType].element == IrType.Primitive(IrPrimitive.U8)
    )

    val tableOperation =
      generation.services.head.operations.find(_.name == "GetEventInitDataLevelTable").get
    assert(tableOperation.pointerChain.nonEmpty)
    assert(tableOperation.pointerChain.get.pointerDepth == 2)
    assert(tableOperation.pointerChain.get.outerArrayLength.contains(2))

    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generation.services)
    assert(plans.errors.isEmpty)
    val tableRoute = plans.routes("/api/MeleeApi/event_init_data_level_table")
    assert(tableRoute.pointerChain.nonEmpty)
    assert(tableRoute.reads.head.sizeBytes == 8)

    val mappingRoute =
      plans.routes("/api/MeleeApi/event_match_selection_index_to_event_match_id_mapping")
    assert(mappingRoute.reads.head.sizeBytes == 0x33)
  }

  test("marks char pointer array chains to follow C strings") {
    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture-char-pointer-array")
    val generation = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot),
        namespace = "example.doldecomp.charptr",
        serviceName = "DolDecompApi",
        wordSizeBits = 32
      )
      .toOption
      .get

    assert(generation.warnings.isEmpty)
    val operation = generation.services.head.operations.find(_.name == "GetDbPokemonNames").get
    assert(operation.pointerChain.exists(_.followCString))
    assert(operation.output.members.head.primitiveOverride.contains(IrPrimitive.Char))

    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generation.services)
    assert(plans.routes("/api/DolDecompApi/db_PokemonNames").pointerChain.exists(_.followCString))
    assert(!plans.routes("/api/DolDecompApi/db_PokemonNames").reads.head.cStringPointer)
  }

  test("serves char pointer array sub-routes as full C strings over HTTP") {
    import cats.effect.IO
    import cats.effect.Ref
    import cats.effect.unsafe.implicits.global
    import org.http4s.Method
    import org.http4s.Request
    import org.http4s.Status
    import org.http4s.implicits._
    import java.util.Base64

    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture-char-pointer-array")
    val generation = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot),
        namespace = "example.doldecomp.charptr",
        serviceName = "DolDecompApi",
        wordSizeBits = 32
      )
      .toOption
      .get
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generation.services)
    val baseAddress = 0x803eaa50L
    val stringAddresses = Map(
      0x80000000L -> "Random",
      0x80000010L -> "Tosakinto",
      0x80000020L -> "Chicorita",
      0x80000030L -> "Kabigon",
      0x80000040L -> "Kamex"
    )
    val memory = scala.collection.mutable.Map.empty[Long, Byte]

    def storePointer(address: Long, pointer: Long): Unit = {
      val bytes = Array[Byte](
        ((pointer >> 24) & 0xff).toByte,
        ((pointer >> 16) & 0xff).toByte,
        ((pointer >> 8) & 0xff).toByte,
        (pointer & 0xff).toByte
      )
      bytes.zipWithIndex.foreach { case (byte, index) =>
        memory(address + index) = byte
      }
    }

    stringAddresses.foreach { case (address, value) =>
      value.getBytes("US-ASCII").zipWithIndex.foreach { case (byte, index) =>
        memory(address + index) = byte
      }
      memory(address + value.length) = 0
    }
    stringAddresses.keys.toArray.sorted.zipWithIndex.foreach { case (pointer, index) =>
      storePointer(baseAddress + index * 4L, pointer)
    }

    def readBytes(address: Long, sizeBytes: Int): Array[Byte] =
      (0 until sizeBytes).map(offset => memory.getOrElse(address + offset, 0.toByte)).toArray

    val dapClient = new DapHttpServerMain.DapClient {
      override def readMemory(address: Long, sizeBytes: Int): IO[Either[String, String]] =
        IO.pure(Right(Base64.getEncoder.encodeToString(readBytes(address, sizeBytes))))

      override def continueExecution(): IO[Either[String, Json]] =
        IO.pure(Right(Json.obj()))
    }

    val plansRef = Ref.unsafe[IO, RoutePlansLoadResult](plans)
    val app = DapHttpServerMain.routes(plansRef, dapClient).orNotFound
    val response =
      app.run(Request[IO](Method.GET, uri"/api/DolDecompApi/db_PokemonNames/2")).unsafeRunSync()
    val body = response.body.compile.toVector.unsafeRunSync().map(_.toChar).mkString
    val decoded = io.circe.parser.parse(body).toOption.get.hcursor.downField("decoded").as[String]

    assert(response.status == Status.Ok)
    assert(decoded.contains("Chicorita"))
  }

  test("groups C bitfields into bitmask structs and bool members") {
    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture-bitfields")
    val generation = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot),
        namespace = "example.doldecomp.bitfields",
        serviceName = "DolDecompApi",
        wordSizeBits = 32
      )
      .toOption
      .get

    assert(generation.warnings.isEmpty)
    val struct =
      generation.services.head.operations.head.output.members.head.target
        .asInstanceOf[IrType.MemoryMappedStruct]
    val x0 = struct.members.find(_.name == "x0").get
    val isTeams = struct.members.find(_.name == "isTeams").get

    assert(x0.target.isInstanceOf[IrType.Bitmask])
    assert(x0.target.asInstanceOf[IrType.Bitmask].members.size == 8)
    assert(x0.target.asInstanceOf[IrType.Bitmask].declaredSizeBits.contains(8))
    assert(isTeams.target == IrType.Primitive(IrPrimitive.Bool))
    assert(isTeams.layoutBitWidth.contains(8))

    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generation.services)
    assert(plans.errors.isEmpty)
    assert(plans.routes("/api/DolDecompApi/start_event_rules").reads.head.decodeCodec.nonEmpty)
  }

  test("incomplete u32 bitfield units use bits-used size (StartMeleeRules-like)") {
    val tmp = java.nio.file.Files.createTempDirectory("dap-u32-bitfields")
    try {
      val hdr = tmp.resolve("rules.h")
      val symbols = tmp.resolve("symbols.txt")
      java.nio.file.Files.writeString(
        hdr,
        """
          |typedef unsigned char u8;
          |typedef unsigned short u16;
          |typedef unsigned int u32;
          |typedef signed int s32;
          |typedef float f32;
          |struct StartMeleeRules {
          |    u32 x0_0 : 3;
          |    u32 x0_3 : 3;
          |    u32 x0_6 : 1;
          |    u32 x0_7 : 1;
          |    u32 x1_0 : 1;
          |    u32 x1_1 : 1;
          |    u32 x1_2 : 1;
          |    u32 x1_3 : 1;
          |    u32 x1_4 : 1;
          |    u32 x1_5 : 1;
          |    u32 x1_6 : 1;
          |    u32 x1_7 : 1;
          |    u32 x2_0 : 1;
          |    u32 x2_1 : 1;
          |    u32 x2_2 : 1;
          |    u32 x2_3 : 1;
          |    u32 x2_4 : 1;
          |    u32 x2_5 : 1;
          |    u32 x2_6 : 1;
          |    u32 x2_7 : 1;
          |    u32 x3_0 : 1;
          |    u32 x3_1 : 1;
          |    u32 x3_2 : 1;
          |    u32 x3_3 : 1;
          |    u32 x3_4 : 1;
          |    u32 x3_5 : 1;
          |    u32 x3_6 : 1;
          |    u32 x3_7 : 1;
          |    u32 x4_0 : 1;
          |    u32 x4_1 : 1;
          |    u32 x4_2 : 1;
          |    u32 x4_3 : 1;
          |    u32 x4_4 : 1;
          |    u32 x4_5 : 1;
          |    u32 x4_6 : 1;
          |    u32 x4_7 : 1;
          |    u32 x5_0 : 1;
          |    u32 x5_1 : 1;
          |    u32 x5_2 : 1;
          |    u32 x5_3 : 1;
          |    u32 x5_4 : 1;
          |    u32 x5_5 : 1;
          |    u32 x5_6 : 1;
          |    u32 x5_7 : 1;
          |    u8 x6;
          |    u8 x7;
          |    u8 is_teams;
          |    u8 x9;
          |    u8 xA;
          |    u8 xB;
          |    u8 xC;
          |    u8 xD;
          |    u16 xE;
          |    u32 x10;
          |    u8 x14;
          |    u32 x18;
          |    u32 x1C_pad[(0x20 - 0x1C) / 4];
          |    u64 x20;
          |    s32 x28;
          |    f32 x2C;
          |    f32 x30;
          |    f32 x34;
          |    void (*on_unpause_override)(int);
          |    void (*on_pause_override)(int);
          |    int (*check_for_pauser_override)(void);
          |    void (*x44)(void);
          |    void (*x48)(void);
          |    void (*x4C)(void);
          |    void (*x50)(u8);
          |    u32* x54;
          |    u32* x58;
          |    u8 pad_x5C[0x60 - 0x5C];
          |};
          |struct StartMeleeRules g_rules;
          |""".stripMargin
      )
      java.nio.file.Files.writeString(
        symbols,
        "g_rules = .data:0x80000000; // type:object size:0x60 scope:global ctype:StartMeleeRules\n"
      )

      val generation = DoldecompIrGenerator
        .generateFromPaths(symbols, List(tmp), "example.rules", "Api", 32)
        .toOption
        .get
      val root = generation.services.head.operations.head.output.members.head.target
        .asInstanceOf[IrType.MemoryMappedStruct]
      assert(root.declaredSizeBits.contains(0x60), s"sizeof=${root.declaredSizeBits}")
      assert(root.members.find(_.name == "x6").flatMap(_.offsetBytes).contains(6))
      assert(root.members.find(_.name == "x20").flatMap(_.offsetBytes).contains(0x20))
      assert(root.members.find(_.name == "onUnpauseOverride").flatMap(_.offsetBytes).contains(0x38))
      assert(root.members.find(_.name == "padX5C").flatMap(_.offsetBytes).contains(0x5c))

      val codec = HttpRouteIrEmitter.compileCodec(root, IrEndian.Big, Some(32))
      assert(codec.isRight, codec)
      val decoded =
        codec.toOption.get.decode(scodec.bits.BitVector(Array.fill[Byte](0x60)(0)))
      assert(decoded.isSuccessful, decoded)
    } finally {
      java.nio.file.Files
        .walk(tmp)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach { p =>
          val _ = java.nio.file.Files.deleteIfExists(p)
        }
    }
  }

  test("packs structs from member types; offset comments are documentation") {
    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture-offsets")
    val generation = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot),
        namespace = "example.doldecomp.offsets",
        serviceName = "DolDecompApi",
        wordSizeBits = 32
      )
      .toOption
      .get

    assert(generation.warnings.isEmpty)
    val struct =
      generation.services.head.operations.head.output.members.head.target
        .asInstanceOf[IrType.MemoryMappedStruct]
    val aMember = struct.members.find(_.name == "a").get
    val bMember = struct.members.find(_.name == "b").get

    assert(aMember.offsetBytes.contains(0x00))
    assert(bMember.offsetBytes.contains(0x04))
    assert(struct.declaredSizeBits.contains(8))

    val outputMember = generation.services.head.operations.head.output.members.head
    assert(outputMember.readSizeBytes.contains(0x08))

    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generation.services)
    val route = plans.routes("/api/DolDecompApi/padded_struct")
    assert(route.reads.head.sizeBytes == 0x08)

    val payload = Array[Byte](0x2a, 0, 0, 0, 0x00, 0x00, 0x12, 0x34)
    val decoded =
      route.reads.head.decodeCodec.get.decode(scodec.bits.BitVector(payload)).toOption.get.value

    assert(decoded.hcursor.downField("a").as[Long].toOption.contains(0x2aL))
    assert(decoded.hcursor.downField("b").as[Long].toOption.contains(0x1234L))
  }

  test("warns when offset comments disagree with type-packed layout") {
    val packed = List(
      IrMember(
        id = ShapeId.from("example#Padded_a"),
        name = "a",
        target = IrType.Primitive(IrPrimitive.U8),
        staticAddress = None,
        paddingRepeats = None,
        isPointer = false,
        isArray = false,
        arrayLength = None,
        endianOverride = None,
        primitiveOverride = Some(IrPrimitive.U8),
        offsetBytes = Some(0)
      ),
      IrMember(
        id = ShapeId.from("example#Padded_b"),
        name = "b",
        target = IrType.Primitive(IrPrimitive.U32),
        staticAddress = None,
        paddingRepeats = None,
        isPointer = false,
        isArray = false,
        arrayLength = None,
        endianOverride = None,
        primitiveOverride = Some(IrPrimitive.U32),
        offsetBytes = Some(4)
      )
    )
    val warnings = DoldecompIrGenerator.commentOffsetWarnings(
      "PaddedStruct",
      List("a", "b"),
      packed,
      Map(("PaddedStruct", "a") -> 0, ("PaddedStruct", "b") -> 0x08)
    )
    assert(warnings.size == 1)
    assert(warnings.head.contains("PaddedStruct.b"))
    assert(warnings.head.contains("0x8"))
    assert(warnings.head.contains("0x4"))
  }

  test("decodes struct char pointer array members as arrays of C strings") {
    import cats.effect.IO
    import cats.effect.Ref
    import cats.effect.unsafe.implicits.global
    import org.http4s.Method
    import org.http4s.Request
    import org.http4s.Status
    import org.http4s.implicits._
    import java.util.Base64

    val fixtureRoot = Paths.get("src/test/resources/doldecomp-fixture-struct-string-array")
    val generation = DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot),
        namespace = "example.doldecomp.structcharptr",
        serviceName = "DolDecompApi",
        wordSizeBits = 32
      )
      .toOption
      .get

    assert(generation.warnings.isEmpty)
    val operation = generation.services.head.operations.head
    assert(operation.name == "GetGStringTable")
    val struct = operation.output.members.head.target.asInstanceOf[IrType.MemoryMappedStruct]
    val namesMember = struct.members.find(_.name == "names").get
    assert(namesMember.isPointer)
    assert(namesMember.isArray)
    assert(namesMember.arrayLength.contains(5))
    assert(namesMember.primitiveOverride.contains(IrPrimitive.Char))
    assert(namesMember.target.isInstanceOf[IrType.ListType])
    assert(
      namesMember.target.asInstanceOf[IrType.ListType].element == IrType.Primitive(
        IrPrimitive.Char
      )
    )

    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generation.services)
    assert(plans.errors.isEmpty)
    val route = plans.routes("/api/DolDecompApi/gStringTable")
    assert(route.reads.head.sizeBytes == 20)

    val baseAddress = 0x80400000L
    val stringAddresses = Map(
      0x80001000L -> "Random",
      0x80001010L -> "Tosakinto",
      0x80001020L -> "Chicorita",
      0x80001030L -> "Kabigon",
      0x80001040L -> "Kamex"
    )
    val memory = scala.collection.mutable.Map.empty[Long, Byte]

    def storePointer(address: Long, pointer: Long): Unit = {
      val bytes = Array[Byte](
        ((pointer >> 24) & 0xff).toByte,
        ((pointer >> 16) & 0xff).toByte,
        ((pointer >> 8) & 0xff).toByte,
        (pointer & 0xff).toByte
      )
      bytes.zipWithIndex.foreach { case (byte, index) =>
        memory(address + index) = byte
      }
    }

    stringAddresses.foreach { case (address, value) =>
      value.getBytes("US-ASCII").zipWithIndex.foreach { case (byte, index) =>
        memory(address + index) = byte
      }
      memory(address + value.length) = 0
    }
    stringAddresses.keys.toArray.sorted.zipWithIndex.foreach { case (pointer, index) =>
      storePointer(baseAddress + index * 4L, pointer)
    }

    def readBytes(address: Long, sizeBytes: Int): Array[Byte] =
      (0 until sizeBytes).map(offset => memory.getOrElse(address + offset, 0.toByte)).toArray

    val dapClient = new DapHttpServerMain.DapClient {
      override def readMemory(address: Long, sizeBytes: Int): IO[Either[String, String]] =
        IO.pure(Right(Base64.getEncoder.encodeToString(readBytes(address, sizeBytes))))

      override def continueExecution(): IO[Either[String, Json]] =
        IO.pure(Right(Json.obj()))
    }

    val plansRef = Ref.unsafe[IO, RoutePlansLoadResult](plans)
    val app = DapHttpServerMain.routes(plansRef, dapClient).orNotFound
    val response =
      app.run(Request[IO](Method.GET, uri"/api/DolDecompApi/gStringTable")).unsafeRunSync()
    val body = response.body.compile.toVector.unsafeRunSync().map(_.toChar).mkString
    val namesJson = io.circe.parser
      .parse(body)
      .toOption
      .get
      .hcursor
      .downField("reads")
      .downN(0)
      .downField("decoded")
      .as[Json]
      .toOption
      .get
      .hcursor
      .downField("names")
      .as[List[String]]
      .toOption
      .get

    assert(response.status == Status.Ok)
    assert(namesJson == List("Random", "Tosakinto", "Chicorita", "Kabigon", "Kamex"))
  }

  test("resolves char* fields inside global struct array elements") {
    import cats.effect.IO
    import cats.effect.Ref
    import cats.effect.unsafe.implicits.global
    import org.http4s.Method
    import org.http4s.Request
    import org.http4s.Status
    import org.http4s.implicits._
    import software.amazon.smithy.model.shapes.ShapeId
    import java.util.Base64

    def sid(name: String): ShapeId = ShapeId.from(s"example#$name")

    val entry = IrType.MemoryMappedStruct(
      id = sid("NamedEntry"),
      members = List(
        IrMember(
          id = sid("NamedEntry$name"),
          name = "name",
          target = IrType.Primitive(IrPrimitive.Char),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = true,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = Some(IrPrimitive.Char),
          offsetBytes = Some(0)
        )
      ),
      declaredSizeBits = Some(4)
    )
    val listType =
      IrType.ListType(
        id = sid("NamedEntryList"),
        element = entry,
        bytesAlias = false,
        bitsAlias = false
      )
    val output = IrType.EnclosingStruct(
      id = sid("NamedEntriesOutput"),
      members = List(
        IrMember(
          id = sid("NamedEntriesOutput$value"),
          name = "value",
          target = listType,
          staticAddress = Some(0x80400000L),
          paddingRepeats = None,
          isPointer = false,
          isArray = true,
          arrayLength = Some(2),
          endianOverride = None,
          primitiveOverride = None,
          readSizeBytes = Some(8)
        )
      ),
      declaredSizeBits = None
    )
    val services = List(
      IrService(
        name = "Api",
        wordSizeBits = Some(32),
        defaultEndian = IrEndian.Big,
        operations =
          List(IrOperation(name = "GetEntries", routePath = "/api/Api/GetEntries", output = output))
      )
    )
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(services)
    assert(plans.errors.isEmpty)

    val memory = scala.collection.mutable.Map.empty[Long, Byte]
    def storePointer(address: Long, pointer: Long): Unit = {
      Array[Byte](
        ((pointer >> 24) & 0xff).toByte,
        ((pointer >> 16) & 0xff).toByte,
        ((pointer >> 8) & 0xff).toByte,
        (pointer & 0xff).toByte
      ).zipWithIndex.foreach { case (byte, index) => memory(address + index) = byte }
    }
    def storeString(address: Long, value: String): Unit = {
      value.getBytes("US-ASCII").zipWithIndex.foreach { case (byte, index) =>
        memory(address + index) = byte
      }
      memory(address + value.length) = 0
    }
    storeString(0x80001000L, "Alpha")
    storeString(0x80001010L, "Beta")
    storePointer(0x80400000L, 0x80001000L)
    storePointer(0x80400004L, 0x80001010L)

    val dapClient = new DapHttpServerMain.DapClient {
      override def readMemory(address: Long, sizeBytes: Int): IO[Either[String, String]] =
        IO.pure(
          Right(
            Base64.getEncoder.encodeToString(
              (0 until sizeBytes)
                .map(offset => memory.getOrElse(address + offset, 0.toByte))
                .toArray
            )
          )
        )
      override def continueExecution(): IO[Either[String, Json]] =
        IO.pure(Right(Json.obj()))
    }

    val plansRef = Ref.unsafe[IO, RoutePlansLoadResult](plans)
    val app = DapHttpServerMain.routes(plansRef, dapClient).orNotFound
    val response =
      app.run(Request[IO](Method.GET, uri"/api/Api/GetEntries")).unsafeRunSync()
    val body = response.body.compile.toVector.unsafeRunSync().map(_.toChar).mkString
    val decoded = io.circe.parser
      .parse(body)
      .toOption
      .get
      .hcursor
      .downField("reads")
      .downN(0)
      .downField("decoded")
      .as[List[Json]]
      .toOption
      .get

    assert(response.status == Status.Ok)
    assert(decoded(0).hcursor.downField("name").as[String].toOption.contains("Alpha"))
    assert(decoded(1).hcursor.downField("name").as[String].toOption.contains("Beta"))
  }

  test("mergeGlobalDeclarations prefers non-static definition over pointer forward decl") {
    val merged = DoldecompIrGenerator.mergeGlobalDeclarations(
      List(
        GlobalVariableDeclaration(
          name = "g_table",
          typeName = "u8",
          isArray = false,
          declaratorLength = None,
          initializerLength = None,
          pointerDepth = 1,
          isStatic = true
        ),
        GlobalVariableDeclaration(
          name = "g_table",
          typeName = "GameScene",
          isArray = true,
          declaratorLength = None,
          initializerLength = Some(2),
          pointerDepth = 0,
          isStatic = false
        )
      )
    )

    assert(merged.typeName == "GameScene")
    assert(merged.isArray)
    assert(merged.initializerLength.contains(2))
    assert(merged.pointerDepth == 0)
    assert(!merged.isStatic)
  }

  test("mergeGlobalDeclarations keeps preferred scalar definition scalar") {
    val merged = DoldecompIrGenerator.mergeGlobalDeclarations(
      List(
        GlobalVariableDeclaration(
          name = "g_state",
          typeName = "State",
          isArray = true,
          declaratorLength = Some(4),
          initializerLength = None,
          pointerDepth = 0,
          isStatic = true
        ),
        GlobalVariableDeclaration(
          name = "g_state",
          typeName = "State",
          isArray = false,
          declaratorLength = None,
          initializerLength = None,
          pointerDepth = 0,
          isStatic = false
        )
      )
    )

    assert(!merged.isArray)
    assert(merged.declaratorLength.isEmpty)
    assert(merged.initializerLength.isEmpty)
    assert(!merged.isStatic)
  }

  test("does not infer an unsized aggregate array length from symbol size") {
    val tmp = java.nio.file.Files.createTempDirectory("dap-aggregate-array-stride")
    try {
      val source = tmp.resolve("data.c")
      val symbols = tmp.resolve("symbols.txt")
      java.nio.file.Files.writeString(
        source,
        """
          |typedef struct Item {
          |    u32 value;
          |} Item;
          |extern Item g_items[];
          |u32 g_count;
          |""".stripMargin
      )
      java.nio.file.Files.writeString(
        symbols,
        """g_items = .data:0x80000000; // type:object size:0x10 scope:global
          |g_count = .data:0x80000010; // type:object size:0x4 scope:global
          |""".stripMargin
      )

      val generation = DoldecompIrGenerator
        .generateFromPaths(
          symbolsPath = symbols,
          headerRoots = List(tmp),
          namespace = "example.aggregate.stride",
          serviceName = "Api",
          wordSizeBits = 32
        )
        .toOption
        .get

      assert(
        generation.warnings.exists(w =>
          w.contains("g_items") && w.contains("symbol size may include element-stride padding")
        )
      )
      assert(
        !generation.services.head.operations.exists(
          _.output.members.exists(_.staticAddress.contains(0x80000000L))
        )
      )
      assert(
        generation.services.head.operations.exists(
          _.output.members.exists(_.staticAddress.contains(0x80000010L))
        )
      )
    } finally {
      java.nio.file.Files.walk(tmp).sorted(java.util.Comparator.reverseOrder()).forEach { path =>
        val _ = java.nio.file.Files.deleteIfExists(path)
      }
    }
  }

  test("warns on conflicting multi-file macros and keeps the first") {
    val tmp = java.nio.file.Files.createTempDirectory("dap-macro-merge")
    try {
      val first = tmp.resolve("a.h")
      val second = tmp.resolve("b.h")
      val source = tmp.resolve("data.c")
      val symbols = tmp.resolve("symbols.txt")
      java.nio.file.Files.writeString(first, "#define LEN 2\n")
      java.nio.file.Files.writeString(second, "#define LEN 4\n")
      java.nio.file.Files.writeString(
        source,
        """
          |u8 g_bytes[LEN];
          |""".stripMargin
      )
      java.nio.file.Files.writeString(
        symbols,
        "g_bytes = .data:0x80000000; // type:object size:0x2 scope:global\n"
      )

      val generation = DoldecompIrGenerator
        .generateFromPaths(
          symbolsPath = symbols,
          headerRoots = List(tmp),
          namespace = "example.macro.merge",
          serviceName = "Api",
          wordSizeBits = 32
        )
        .toOption
        .get

      assert(generation.warnings.exists(w => w.contains("LEN") && w.contains("Conflicting macro")))
      val valueMember = generation.services.head.operations.head.output.members.head
      assert(valueMember.arrayLength.contains(2))
    } finally {
      java.nio.file.Files.walk(tmp).sorted(java.util.Comparator.reverseOrder()).forEach { path =>
        val _ = java.nio.file.Files.deleteIfExists(path)
      }
    }
  }

  test("resolves pointer-array length from enumerator bound across headers") {
    val tmp = java.nio.file.Files.createTempDirectory("dap-enum-array-bound")
    try {
      val forward = tmp.resolve("forward.h")
      val types = tmp.resolve("types.h")
      val symbols = tmp.resolve("symbols.txt")
      java.nio.file.Files.writeString(
        forward,
        """
          |typedef enum Place {
          |    Hundreds,
          |    Tens,
          |    Ones,
          |    Percent,
          |    HUD_PLACE_MAX
          |} Place;
          |""".stripMargin
      )
      java.nio.file.Files.writeString(
        types,
        """
          |typedef struct Hud {
          |    int* jobjs[HUD_PLACE_MAX];
          |} Hud;
          |Hud ifStatus_HudInfo;
          |""".stripMargin
      )
      java.nio.file.Files.writeString(
        symbols,
        "ifStatus_HudInfo = .data:0x80000000; // type:object size:0x10 scope:global\n"
      )

      val generation = DoldecompIrGenerator
        .generateFromPaths(
          symbolsPath = symbols,
          headerRoots = List(tmp),
          namespace = "example.enum.array",
          serviceName = "Api",
          wordSizeBits = 32
        )
        .toOption
        .get

      val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generation.services)
      assert(plans.errors.isEmpty, plans.errors.mkString(", "))
      val hud = generation.services.head.operations.head.output.members.head.target
        .asInstanceOf[IrType.MemoryMappedStruct]
      val jobjs = hud.members.find(_.name == "jobjs").get
      assert(jobjs.isArray)
      assert(jobjs.isPointer)
      assert(jobjs.arrayLength.contains(4))
    } finally {
      java.nio.file.Files.walk(tmp).sorted(java.util.Comparator.reverseOrder()).forEach { path =>
        val _ = java.nio.file.Files.deleteIfExists(path)
      }
    }
  }

  test("suggests nearby include paths when missing types fail without auto-adding them") {
    val tmp = java.nio.file.Files.createTempDirectory("dap-speculate-includes")
    try {
      val src = tmp.resolve("src")
      val sdk = tmp.resolve("extern/dolphin/include")
      java.nio.file.Files.createDirectories(src)
      java.nio.file.Files.createDirectories(sdk)
      java.nio.file.Files.writeString(
        src.resolve("game.h"),
        """
          |Vec3 g_vec;
          |GXColor g_color;
          |""".stripMargin
      )
      java.nio.file.Files.writeString(
        sdk.resolve("math.h"),
        """
          |typedef struct Vec3 {
          |    float x, y, z;
          |} Vec3;
          |""".stripMargin
      )
      java.nio.file.Files.writeString(
        sdk.resolve("gx.h"),
        """
          |typedef struct GXColor {
          |    unsigned char r, g, b, a;
          |} GXColor;
          |""".stripMargin
      )
      val symbols = tmp.resolve("symbols.txt")
      java.nio.file.Files.writeString(
        symbols,
        """
          |g_vec = .data:0x80000000; // type:object size:0xc scope:global
          |g_color = .data:0x80000010; // type:object size:0x4 scope:global
          |""".stripMargin
      )

      val generation = DoldecompIrGenerator
        .generateFromPaths(
          symbolsPath = symbols,
          headerRoots = List(src),
          namespace = "example.speculate",
          serviceName = "Api",
          wordSizeBits = 32
        )
        .toOption
        .get

      assert(generation.services.isEmpty || generation.services.head.operations.isEmpty)
      assert(generation.warnings.exists(_.contains("Vec3")))
      assert(generation.warnings.exists(_.contains("GXColor")))
      assert(
        generation.warnings.exists(w =>
          w.contains("Nearby paths") && w.contains("extern/dolphin/include") && w.contains(
            "--headers"
          )
        )
      )
    } finally {
      java.nio.file.Files.walk(tmp).sorted(java.util.Comparator.reverseOrder()).forEach { path =>
        val _ = java.nio.file.Files.deleteIfExists(path)
      }
    }
  }

  test("warns when symbol size is inconsistent with C-derived array length") {
    val tmp = java.nio.file.Files.createTempDirectory("dap-size-mismatch")
    try {
      val header = tmp.resolve("types.h")
      val source = tmp.resolve("data.c")
      val symbols = tmp.resolve("symbols.txt")
      java.nio.file.Files.writeString(
        header,
        """
          |typedef struct Item {
          |    u32 value;
          |} Item;
          |""".stripMargin
      )
      java.nio.file.Files.writeString(
        source,
        """
          |#include "types.h"
          |Item g_items[2];
          |""".stripMargin
      )
      // 3 bytes is too small for Item[2] (element size 4 → need at least 8).
      java.nio.file.Files.writeString(
        symbols,
        "g_items = .data:0x80000000; // type:object size:0x3 scope:global\n"
      )

      val generation = DoldecompIrGenerator
        .generateFromPaths(
          symbolsPath = symbols,
          headerRoots = List(tmp),
          namespace = "example.size.mismatch",
          serviceName = "Api",
          wordSizeBits = 32
        )
        .toOption
        .get

      assert(
        generation.warnings.exists(w =>
          w.contains("g_items") && w.contains("inconsistent with array length")
        )
      )
    } finally {
      java.nio.file.Files.walk(tmp).sorted(java.util.Comparator.reverseOrder()).forEach { path =>
        val _ = java.nio.file.Files.deleteIfExists(path)
      }
    }
  }
}
