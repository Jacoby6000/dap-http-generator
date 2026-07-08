package io.github.jacoby6000.daphttp

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
    val route = plans("/MeleeApi/GetGPlayerState")
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
    val playerRoute = plans("/MeleeApi/GetGPlayerState")
    val worldRoute = plans("/MeleeApi/GetGWorldState")

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
    val route = plans("/MeleeApi/GetGPlayerState")

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
    val route = plans("/MeleeApi/GetGm803DDAC0Scenes")

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
    assert(plans.routes.contains("/MeleeApi/GetGPlayerState"))
    assert(!plans.routes.contains("/MeleeApi/GetBadSymbol"))
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
    assert(plans.routes.contains("/MeleeApi/GetIntVar"))
    assert(plans.routes("/MeleeApi/GetIntVar").reads.head.sizeBytes == 4)
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
    assert(plans.routes.contains("/StringsApi/GetGStringFields"))
    assert(plans.routes("/StringsApi/GetGStringFields").reads.head.decodeCodec.nonEmpty)
  }
}
