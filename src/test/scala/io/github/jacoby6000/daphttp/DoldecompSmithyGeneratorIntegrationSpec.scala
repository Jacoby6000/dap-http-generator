package io.github.jacoby6000.daphttp

import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

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
    assert(valueMember.isArray)
    assert(valueMember.arrayLength.contains(2))

    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(irServices).routes
    val route = plans("/api/MeleeApi/gm_803DDAC0_Scenes")

    assert(route.reads.head.address == 0x803ddac0L)
    assert(route.reads.head.sizeBytes == 42)
    assert(route.reads.head.decodeCodec.nonEmpty)
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
    assert(generation.services.head.operations.map(_.name) == List("GetGPlayerState"))

    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generation.services)
    assert(plans.routes.contains("/api/MeleeApi/gPlayerState"))
    assert(!plans.routes.contains("/api/MeleeApi/badSymbol"))
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
}
