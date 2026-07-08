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

    val plans = IrCompiler.compileRoutePlansFromIr(irServices).toOption.get
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

    val service = irServices.head
    val plans = IrCompiler.compileRoutePlansFromIr(irServices).toOption.get
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
      inventoryMember.target.asInstanceOf[IrType.ListType].element
        .asInstanceOf[IrType.Struct]
        .members
        .exists(_.name == "quantity")
    )
    assert(scratchMember.isPointer)
    assert(scratchMember.target == IrType.Primitive(IrPrimitive.LongWord))
  }
}
