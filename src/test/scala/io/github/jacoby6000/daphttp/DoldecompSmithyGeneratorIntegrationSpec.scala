package io.github.jacoby6000.daphttp

import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite
import software.amazon.smithy.model.Model

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
    assert(route.reads.head.sizeBytes == 32)
    assert(route.reads.head.decodeCodec.nonEmpty)
    assert(!scoreMember.isPointer)
    assert(scoreMember.target == IrType.Primitive(IrPrimitive.U128))

    val payload = Array[Byte](
      0x00,
      0x00,
      0x00,
      0x2a,
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
    assert(route.reads.head.sizeBytes == 42)
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

    assert(original.head.operations.head.output.members.head.readSizeBytes.contains(0x0c))
    assert(roundTripped.head.operations.head.output.members.head.readSizeBytes.contains(0x0c))

    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(roundTripped)
    assert(plans.routes("/api/DolDecompApi/padded_struct").reads.head.sizeBytes == 0x0c)
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
      generation.warnings.exists(w => w.contains("textObject") && w.contains(".text"))
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

    assert(generation.warnings.exists(_.contains("Color: Conflicting enum definitions")))
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

  test("honors doldecomp member offset comments in struct layout") {
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
    assert(bMember.offsetBytes.contains(0x08))

    val outputMember = generation.services.head.operations.head.output.members.head
    assert(outputMember.readSizeBytes.contains(0x0c))

    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generation.services)
    val route = plans.routes("/api/DolDecompApi/padded_struct")
    assert(route.reads.head.sizeBytes == 0x0c)

    val payload = Array.fill[Byte](12)(0)
    payload(0) = 0x2a
    payload(10) = 0x12
    payload(11) = 0x34
    val decoded =
      route.reads.head.decodeCodec.get.decode(scodec.bits.BitVector(payload)).toOption.get.value

    assert(decoded.hcursor.downField("a").as[Long].toOption.contains(0x2aL))
    assert(decoded.hcursor.downField("b").as[Long].toOption.contains(0x1234L))
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
