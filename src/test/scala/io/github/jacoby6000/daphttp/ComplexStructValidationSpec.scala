package io.github.jacoby6000.daphttp

import cats.effect.IO
import cats.effect.Ref
import cats.effect.unsafe.implicits.global
import io.circe.Json
import org.http4s.Method
import org.http4s.Request
import org.http4s.Status
import org.http4s.Uri
import org.http4s.implicits._
import org.scalatest.funsuite.AnyFunSuite
import software.amazon.smithy.model.Model

import java.nio.file.Paths
import java.util.Base64

class ComplexStructValidationSpec extends AnyFunSuite {
  private val fixtureRoot =
    Paths.get("src/test/resources/doldecomp-fixture-complex-struct")

  private val baseAddress = 0x80400000L

  private val stringAddresses: Map[Long, String] = Map(
    0x80001000L -> "Mario",
    0x80002000L -> "Red",
    0x80002010L -> "Blue",
    0x80002020L -> "Green",
    0x80002030L -> "Yellow"
  )

  private val colorAddresses: Map[Long, (Byte, Byte, Byte, Byte)] = Map(
    0x80003000L -> ((0xff.toByte, 0x00, 0x00, 0xff.toByte)),
    0x80003010L -> ((0x00, 0xff.toByte, 0x00, 0xff.toByte)),
    0x80003020L -> ((0x00, 0x00, 0xff.toByte, 0x80.toByte))
  )

  private def buildStructBytes: Array[Byte] = {
    val bytes = new Array[Byte](64)

    def putPointer(offset: Int, value: Long): Unit = {
      bytes(offset) = ((value >> 24) & 0xff).toByte
      bytes(offset + 1) = ((value >> 16) & 0xff).toByte
      bytes(offset + 2) = ((value >> 8) & 0xff).toByte
      bytes(offset + 3) = (value & 0xff).toByte
    }

    def putF32(offset: Int, value: Float): Unit = {
      val raw = java.lang.Float.floatToRawIntBits(value)
      bytes(offset) = ((raw >> 24) & 0xff).toByte
      bytes(offset + 1) = ((raw >> 16) & 0xff).toByte
      bytes(offset + 2) = ((raw >> 8) & 0xff).toByte
      bytes(offset + 3) = (raw & 0xff).toByte
    }

    putPointer(0x00, 0x80001000L)
    Array(0x80002000L, 0x80002010L, 0x80002020L, 0x80002030L).zipWithIndex.foreach {
      case (ptr, i) => putPointer(0x04 + i * 4, ptr)
    }
    bytes(0x14) = 100.toByte
    bytes(0x15) = 75.toByte
    bytes(0x16) = 0
    bytes(0x17) = 0
    bytes(0x18) = 0; bytes(0x19) = 42
    bytes(0x1a) = 0; bytes(0x1b) = 7
    putF32(0x1c, 0.08f)
    putF32(0x20, 1.5f)
    putF32(0x24, 2.0f)
    putF32(0x28, -3.5f)
    Array(0x80003000L, 0x80003010L, 0x80003020L).zipWithIndex.foreach { case (ptr, i) =>
      putPointer(0x2c + i * 4, ptr)
    }
    val uid = 0x0123456789abcdefL
    (0 until 8).foreach { i =>
      bytes(0x38 + i) = ((uid >> ((7 - i) * 8)) & 0xff).toByte
    }
    bytes
  }

  private def buildMemoryMap: Map[Long, Byte] = {
    val mem = scala.collection.mutable.Map.empty[Long, Byte]

    buildStructBytes.zipWithIndex.foreach { case (b, i) =>
      mem(baseAddress + i) = b
    }

    stringAddresses.foreach { case (addr, str) =>
      str.getBytes("US-ASCII").zipWithIndex.foreach { case (b, i) =>
        mem(addr + i) = b
      }
      mem(addr + str.length) = 0
    }

    colorAddresses.foreach { case (addr, (r, g, b, a)) =>
      mem(addr) = r
      mem(addr + 1) = g
      mem(addr + 2) = b
      mem(addr + 3) = a
    }

    mem.toMap
  }

  private def mockDapClient(memory: Map[Long, Byte]): DapHttpServerMain.DapClient =
    new DapHttpServerMain.DapClient {
      override def readMemory(
          address: Long,
          sizeBytes: Int
      ): IO[Either[String, String]] = {
        val bytes =
          (0 until sizeBytes).map(offset => memory.getOrElse(address + offset, 0.toByte)).toArray
        IO.pure(Right(Base64.getEncoder.encodeToString(bytes)))
      }

      override def continueExecution(): IO[Either[String, Json]] =
        IO.pure(Right(Json.obj()))
    }

  private def generateIr: List[IrService] =
    DoldecompIrGenerator
      .generateFromPaths(
        symbolsPath = fixtureRoot.resolve("symbols.txt"),
        headerRoots = List(fixtureRoot),
        namespace = "example.complex",
        serviceName = "ComplexApi",
        wordSizeBits = 32
      )
      .toOption
      .get
      .services

  test("complex struct IR has correct member types and layout") {
    val ir = generateIr
    val op = ir.head.operations.head
    assert(op.name == "GetGFighterInfo")
    val struct = op.output.members.head.target.asInstanceOf[IrType.MemoryMappedStruct]
    val members = struct.members.map(m => m.name -> m).toMap

    val name = members("name")
    assert(name.isPointer && !name.isArray)
    assert(name.primitiveOverride.contains(IrPrimitive.Char))
    assert(name.offsetBytes.contains(0x00))

    val costumeNames = members("costumeNames")
    assert(costumeNames.isPointer && costumeNames.isArray)
    assert(costumeNames.arrayLength.contains(4))
    assert(costumeNames.primitiveOverride.contains(IrPrimitive.Char))
    assert(costumeNames.target.isInstanceOf[IrType.ListType])

    val weight = members("weight")
    assert(!weight.isPointer && !weight.isArray)
    assert(weight.primitiveOverride.contains(IrPrimitive.U8))
    assert(weight.offsetBytes.contains(0x14))

    val pad = members("pad")
    assert(!pad.isPointer && pad.isArray)
    assert(pad.arrayLength.contains(2))
    assert(pad.target.isInstanceOf[IrType.ListType])

    val wins = members("wins")
    assert(wins.primitiveOverride.contains(IrPrimitive.S16))
    assert(wins.offsetBytes.contains(0x18))

    val spawnPos = members("spawnPos")
    assert(spawnPos.target.isInstanceOf[IrType.MemoryMappedStruct])
    assert(spawnPos.offsetBytes.contains(0x20))

    val colors = members("colors")
    assert(colors.isPointer && colors.isArray)
    assert(colors.arrayLength.contains(3))
    assert(colors.primitiveOverride.isEmpty)
    assert(colors.target.isInstanceOf[IrType.ListType])

    val uniqueId = members("uniqueId")
    assert(uniqueId.primitiveOverride.contains(IrPrimitive.U64))
    assert(uniqueId.offsetBytes.contains(0x38))
  }

  test("complex struct route reads the full 64-byte struct") {
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generateIr)
    assert(plans.errors.isEmpty)
    val route = plans.routes("/ComplexApi/gFighterInfo")
    assert(route.reads.size == 1)
    assert(route.reads.head.sizeBytes == 64)
    assert(route.reads.head.address == baseAddress)
  }

  test("complex struct decodes all fields correctly via mock DAP") {
    val memory = buildMemoryMap
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generateIr)
    val plansRef = Ref.unsafe[IO, RoutePlansLoadResult](plans)
    val app = DapHttpServerMain.routes(plansRef, mockDapClient(memory)).orNotFound

    val response =
      app.run(Request[IO](Method.GET, uri"/ComplexApi/gFighterInfo")).unsafeRunSync()
    val body = response.body.compile.toVector.unsafeRunSync().map(_.toChar).mkString
    val json = io.circe.parser.parse(body).toOption.get

    assert(response.status == Status.Ok)

    val decoded = json.hcursor.downField("reads").downN(0).downField("decoded")

    assert(decoded.downField("name").as[String].toOption.contains("Mario"))

    val costumes = decoded.downField("costumeNames").as[List[String]].toOption.get
    assert(costumes == List("Red", "Blue", "Green", "Yellow"))

    assert(decoded.downField("weight").as[Int].toOption.contains(100))
    assert(decoded.downField("speed").as[Int].toOption.contains(75))

    val pad = decoded.downField("pad").as[List[Int]].toOption.get
    assert(pad == List(0, 0))

    assert(decoded.downField("wins").as[Int].toOption.contains(42))
    assert(decoded.downField("losses").as[Int].toOption.contains(7))

    val gravity = decoded.downField("gravity").as[Double].toOption.get
    assert(math.abs(gravity - 0.08) < 0.001)

    val spawnPos = decoded.downField("spawnPos")
    assert(math.abs(spawnPos.downField("x").as[Double].toOption.get - 1.5) < 0.001)
    assert(math.abs(spawnPos.downField("y").as[Double].toOption.get - 2.0) < 0.001)
    assert(math.abs(spawnPos.downField("z").as[Double].toOption.get - (-3.5)) < 0.001)

    val colors = decoded.downField("colors").as[List[Long]].toOption.get
    assert(
      colors == List(
        0x80003000L - 0x100000000L,
        0x80003010L - 0x100000000L,
        0x80003020L - 0x100000000L
      )
    )

    val uid = decoded.downField("uniqueId").as[String].toOption.get
    assert(uid == "81985529216486895")
  }

  test("complex struct round-trips through Smithy with equivalent routes") {
    val ir = generateIr
    val smithyText = SmithyIrEmitter.emit(ir).fold(errors => fail(errors.mkString("\n")), identity)
    val model = Model
      .assembler()
      .addImport("src/main/smithy/dap-http-traits.smithy")
      .addUnparsedModel("complex.smithy", smithyText)
      .assemble()
      .unwrap()
    val roundTrippedIr = SmithyIrGenerator
      .generateFromModel(model)
      .fold(errors => fail(errors.mkString("\n")), identity)

    val cheadersPlans = HttpRouteIrEmitter.emitRoutePlansFromIr(ir)
    val smithyPlans = HttpRouteIrEmitter.emitRoutePlansFromIr(roundTrippedIr)

    assert(cheadersPlans.errors.isEmpty)
    assert(smithyPlans.errors.isEmpty)
    assert(cheadersPlans.routes.keySet == smithyPlans.routes.keySet)

    val cheadersRoute = cheadersPlans.routes("/ComplexApi/gFighterInfo")
    val smithyRoute = smithyPlans.routes("/ComplexApi/gFighterInfo")
    assert(cheadersRoute.reads.head.sizeBytes == smithyRoute.reads.head.sizeBytes)
  }

  test("complex struct decodes identically via Smithy round-trip") {
    val ir = generateIr
    val smithyText = SmithyIrEmitter.emit(ir).fold(errors => fail(errors.mkString("\n")), identity)
    val model = Model
      .assembler()
      .addImport("src/main/smithy/dap-http-traits.smithy")
      .addUnparsedModel("complex.smithy", smithyText)
      .assemble()
      .unwrap()
    val roundTrippedIr = SmithyIrGenerator
      .generateFromModel(model)
      .fold(errors => fail(errors.mkString("\n")), identity)

    val memory = buildMemoryMap
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(roundTrippedIr)
    val plansRef = Ref.unsafe[IO, RoutePlansLoadResult](plans)
    val app = DapHttpServerMain.routes(plansRef, mockDapClient(memory)).orNotFound

    val response =
      app.run(Request[IO](Method.GET, uri"/ComplexApi/gFighterInfo")).unsafeRunSync()
    val body = response.body.compile.toVector.unsafeRunSync().map(_.toChar).mkString
    val json = io.circe.parser.parse(body).toOption.get

    assert(response.status == Status.Ok)

    val decoded = json.hcursor.downField("reads").downN(0).downField("decoded")

    assert(decoded.downField("name").as[String].toOption.contains("Mario"))

    val costumes = decoded.downField("costumeNames").as[List[String]].toOption.get
    assert(costumes == List("Red", "Blue", "Green", "Yellow"))

    assert(decoded.downField("weight").as[Int].toOption.contains(100))
    assert(decoded.downField("speed").as[Int].toOption.contains(75))

    assert(decoded.downField("wins").as[Int].toOption.contains(42))
    assert(decoded.downField("losses").as[Int].toOption.contains(7))

    val spawnPos = decoded.downField("spawnPos")
    assert(math.abs(spawnPos.downField("x").as[Double].toOption.get - 1.5) < 0.001)

    val colors = decoded.downField("colors").as[List[Long]].toOption.get
    assert(
      colors == List(
        0x80003000L - 0x100000000L,
        0x80003010L - 0x100000000L,
        0x80003020L - 0x100000000L
      )
    )
  }

  // ---- Member sub-route tests ----

  test("member sub-routes are generated for all struct members") {
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generateIr)
    assert(plans.errors.isEmpty)
    val route = plans.routes("/ComplexApi/gFighterInfo")
    val subRouteNames = route.memberSubRoutes.map(_.memberName).toSet
    assert(
      subRouteNames == Set(
        "name",
        "costumeNames",
        "weight",
        "speed",
        "pad",
        "wins",
        "losses",
        "gravity",
        "spawnPos",
        "colors",
        "uniqueId"
      )
    )
  }

  test("/routes endpoint lists member sub-routes") {
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generateIr)
    val plansRef = Ref.unsafe[IO, RoutePlansLoadResult](plans)
    val app = DapHttpServerMain.routes(plansRef, mockDapClient(buildMemoryMap)).orNotFound

    val response =
      app.run(Request[IO](Method.GET, uri"/routes")).unsafeRunSync()
    assert(response.status == Status.Ok)
    val body = response.body.compile.toVector.unsafeRunSync().map(_.toChar).mkString
    val json = io.circe.parser.parse(body).toOption.get
    val routeList = json.hcursor.downField("routes").as[List[String]].toOption.get

    assert(routeList.contains("/ComplexApi/gFighterInfo/name"))
    assert(routeList.contains("/ComplexApi/gFighterInfo/costumeNames/{index}"))
    assert(routeList.contains("/ComplexApi/gFighterInfo/colors/{index}"))
    assert(routeList.contains("/ComplexApi/gFighterInfo/weight"))
    assert(routeList.contains("/ComplexApi/gFighterInfo/spawnPos"))
    assert(routeList.contains("/ComplexApi/gFighterInfo/uniqueId"))
    assert(routeList.contains("/ComplexApi/gFighterInfo/pad/{index}"))
  }

  test("single char pointer sub-route dereferences to a C string") {
    val memory = buildMemoryMap
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generateIr)
    val plansRef = Ref.unsafe[IO, RoutePlansLoadResult](plans)
    val app = DapHttpServerMain.routes(plansRef, mockDapClient(memory)).orNotFound

    val response =
      app.run(Request[IO](Method.GET, uri"/ComplexApi/gFighterInfo/name")).unsafeRunSync()
    assert(response.status == Status.Ok)
    val body = response.body.compile.toVector.unsafeRunSync().map(_.toChar).mkString
    val json = io.circe.parser.parse(body).toOption.get

    assert(json.hcursor.downField("member").as[String].toOption.contains("name"))
    assert(json.hcursor.downField("index").as[Json].toOption.contains(Json.Null))
    assert(json.hcursor.downField("decoded").as[String].toOption.contains("Mario"))
    assert(
      json.hcursor.downField("pointerAddress").as[String].toOption.contains("0x80400000")
    )
  }

  test("char pointer array sub-route dereferences each element to a C string") {
    val memory = buildMemoryMap
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generateIr)
    val plansRef = Ref.unsafe[IO, RoutePlansLoadResult](plans)
    val app = DapHttpServerMain.routes(plansRef, mockDapClient(memory)).orNotFound

    val expectedCostumes = List("Red", "Blue", "Green", "Yellow")
    expectedCostumes.zipWithIndex.foreach { case (expected, idx) =>
      val response = app
        .run(
          Request[IO](
            Method.GET,
            Uri.unsafeFromString(s"/ComplexApi/gFighterInfo/costumeNames/$idx")
          )
        )
        .unsafeRunSync()
      assert(response.status == Status.Ok, s"costumeNames/$idx should be Ok")
      val body = response.body.compile.toVector.unsafeRunSync().map(_.toChar).mkString
      val json = io.circe.parser.parse(body).toOption.get

      assert(json.hcursor.downField("member").as[String].toOption.contains("costumeNames"))
      assert(json.hcursor.downField("index").as[Int].toOption.contains(idx))
      assert(json.hcursor.downField("decoded").as[String].toOption.contains(expected))
    }
  }

  test("char pointer array sub-route without index returns 404") {
    val memory = buildMemoryMap
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generateIr)
    val plansRef = Ref.unsafe[IO, RoutePlansLoadResult](plans)
    val app = DapHttpServerMain.routes(plansRef, mockDapClient(memory)).orNotFound

    val response =
      app
        .run(Request[IO](Method.GET, uri"/ComplexApi/gFighterInfo/costumeNames"))
        .unsafeRunSync()
    assert(response.status == Status.NotFound)
  }

  test("struct pointer array sub-route dereferences each element to a struct") {
    val memory = buildMemoryMap
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generateIr)
    val plansRef = Ref.unsafe[IO, RoutePlansLoadResult](plans)
    val app = DapHttpServerMain.routes(plansRef, mockDapClient(memory)).orNotFound

    val expectedColors = List(
      Map("r" -> 255, "g" -> 0, "b" -> 0, "a" -> 255),
      Map("r" -> 0, "g" -> 255, "b" -> 0, "a" -> 255),
      Map("r" -> 0, "g" -> 0, "b" -> 255, "a" -> 128)
    )

    expectedColors.zipWithIndex.foreach { case (expected, idx) =>
      val response = app
        .run(
          Request[IO](Method.GET, Uri.unsafeFromString(s"/ComplexApi/gFighterInfo/colors/$idx"))
        )
        .unsafeRunSync()
      assert(response.status == Status.Ok, s"colors/$idx should be Ok")
      val body = response.body.compile.toVector.unsafeRunSync().map(_.toChar).mkString
      val json = io.circe.parser.parse(body).toOption.get

      assert(json.hcursor.downField("member").as[String].toOption.contains("colors"))
      assert(json.hcursor.downField("index").as[Int].toOption.contains(idx))
      assert(json.hcursor.downField("bytes").as[Int].toOption.contains(4))

      val decoded = json.hcursor.downField("decoded")
      assert(decoded.downField("r").as[Int].toOption.contains(expected("r")))
      assert(decoded.downField("g").as[Int].toOption.contains(expected("g")))
      assert(decoded.downField("b").as[Int].toOption.contains(expected("b")))
      assert(decoded.downField("a").as[Int].toOption.contains(expected("a")))
    }
  }

  test("member sub-route with out-of-bounds index dereferences to zeroed memory") {
    val memory = buildMemoryMap
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generateIr)
    val plansRef = Ref.unsafe[IO, RoutePlansLoadResult](plans)
    val app = DapHttpServerMain.routes(plansRef, mockDapClient(memory)).orNotFound

    val response =
      app
        .run(
          Request[IO](
            Method.GET,
            Uri.unsafeFromString(s"/ComplexApi/gFighterInfo/costumeNames/15")
          )
        )
        .unsafeRunSync()
    assert(response.status == Status.Ok)
    val body = response.body.compile.toVector.unsafeRunSync().map(_.toChar).mkString
    val json = io.circe.parser.parse(body).toOption.get
    assert(json.hcursor.downField("decoded").as[String].toOption.contains(""))
  }

  test("unknown member sub-route returns 404") {
    val memory = buildMemoryMap
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generateIr)
    val plansRef = Ref.unsafe[IO, RoutePlansLoadResult](plans)
    val app = DapHttpServerMain.routes(plansRef, mockDapClient(memory)).orNotFound

    val response =
      app
        .run(Request[IO](Method.GET, uri"/ComplexApi/gFighterInfo/unknownMember"))
        .unsafeRunSync()
    assert(response.status == Status.NotFound)
  }

  test("Smithy round-trip preserves member sub-routes for pointer arrays and single pointers") {
    val ir = generateIr
    val smithyText = SmithyIrEmitter.emit(ir).fold(errors => fail(errors.mkString("\n")), identity)
    val model = Model
      .assembler()
      .addImport("src/main/smithy/dap-http-traits.smithy")
      .addUnparsedModel("complex.smithy", smithyText)
      .assemble()
      .unwrap()
    val roundTrippedIr = SmithyIrGenerator
      .generateFromModel(model)
      .fold(errors => fail(errors.mkString("\n")), identity)

    val cheadersPlans = HttpRouteIrEmitter.emitRoutePlansFromIr(ir)
    val smithyPlans = HttpRouteIrEmitter.emitRoutePlansFromIr(roundTrippedIr)
    assert(cheadersPlans.errors.isEmpty)
    assert(smithyPlans.errors.isEmpty)

    val cheadersRoute = cheadersPlans.routes("/ComplexApi/gFighterInfo")
    val smithyRoute = smithyPlans.routes("/ComplexApi/gFighterInfo")

    val cheadersSubs = cheadersRoute.memberSubRoutes.map(s => s.memberName -> s).toMap
    val smithySubs = smithyRoute.memberSubRoutes.map(s => s.memberName -> s).toMap

    assert(cheadersSubs.keySet == smithySubs.keySet)

    val cheadersName = cheadersSubs("name").asInstanceOf[MemberSubRoute.PointerSubRoute]
    val smithyName = smithySubs("name").asInstanceOf[MemberSubRoute.PointerSubRoute]
    assert(smithyName.followCString == cheadersName.followCString)
    assert(smithyName.isArray == cheadersName.isArray)
    assert(smithyName.memberOffsetBytes == cheadersName.memberOffsetBytes)

    val cheadersCostumes = cheadersSubs("costumeNames").asInstanceOf[MemberSubRoute.PointerSubRoute]
    val smithyCostumes = smithySubs("costumeNames").asInstanceOf[MemberSubRoute.PointerSubRoute]
    assert(smithyCostumes.followCString == cheadersCostumes.followCString)
    assert(smithyCostumes.isArray == cheadersCostumes.isArray)
    assert(smithyCostumes.arrayLength == cheadersCostumes.arrayLength)

    val cheadersColors = cheadersSubs("colors").asInstanceOf[MemberSubRoute.PointerSubRoute]
    val smithyColors = smithySubs("colors").asInstanceOf[MemberSubRoute.PointerSubRoute]
    assert(smithyColors.followCString == cheadersColors.followCString)
    assert(smithyColors.isArray == cheadersColors.isArray)
    assert(smithyColors.arrayLength == cheadersColors.arrayLength)
  }

  test("Smithy round-trip member sub-routes serve identical decoded data") {
    val ir = generateIr
    val smithyText = SmithyIrEmitter.emit(ir).fold(errors => fail(errors.mkString("\n")), identity)
    val model = Model
      .assembler()
      .addImport("src/main/smithy/dap-http-traits.smithy")
      .addUnparsedModel("complex.smithy", smithyText)
      .assemble()
      .unwrap()
    val roundTrippedIr = SmithyIrGenerator
      .generateFromModel(model)
      .fold(errors => fail(errors.mkString("\n")), identity)

    val memory = buildMemoryMap
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(roundTrippedIr)
    val plansRef = Ref.unsafe[IO, RoutePlansLoadResult](plans)
    val app = DapHttpServerMain.routes(plansRef, mockDapClient(memory)).orNotFound

    val nameResponse =
      app.run(Request[IO](Method.GET, uri"/ComplexApi/gFighterInfo/name")).unsafeRunSync()
    assert(nameResponse.status == Status.Ok)
    val nameBody = nameResponse.body.compile.toVector.unsafeRunSync().map(_.toChar).mkString
    val nameJson = io.circe.parser.parse(nameBody).toOption.get
    assert(nameJson.hcursor.downField("decoded").as[String].toOption.contains("Mario"))

    val colorResponse =
      app
        .run(
          Request[IO](Method.GET, Uri.unsafeFromString("/ComplexApi/gFighterInfo/colors/0"))
        )
        .unsafeRunSync()
    assert(colorResponse.status == Status.Ok)
    val colorBody = colorResponse.body.compile.toVector.unsafeRunSync().map(_.toChar).mkString
    val colorJson = io.circe.parser.parse(colorBody).toOption.get
    val decoded = colorJson.hcursor.downField("decoded")
    assert(decoded.downField("r").as[Int].toOption.contains(255))
    assert(decoded.downField("g").as[Int].toOption.contains(0))
    assert(decoded.downField("b").as[Int].toOption.contains(0))
    assert(decoded.downField("a").as[Int].toOption.contains(255))
  }

  // ---- Value sub-route tests (non-pointer member dereference by offset) ----

  test("primitive value sub-route reads only the member bytes at offset") {
    val memory = buildMemoryMap
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generateIr)
    val plansRef = Ref.unsafe[IO, RoutePlansLoadResult](plans)
    val app = DapHttpServerMain.routes(plansRef, mockDapClient(memory)).orNotFound

    val response =
      app.run(Request[IO](Method.GET, uri"/ComplexApi/gFighterInfo/weight")).unsafeRunSync()
    assert(response.status == Status.Ok)
    val body = response.body.compile.toVector.unsafeRunSync().map(_.toChar).mkString
    val json = io.circe.parser.parse(body).toOption.get

    assert(json.hcursor.downField("member").as[String].toOption.contains("weight"))
    assert(json.hcursor.downField("bytes").as[Int].toOption.contains(1))
    assert(json.hcursor.downField("decoded").as[Int].toOption.contains(100))
  }

  test("struct value sub-route reads only the member struct bytes at offset") {
    val memory = buildMemoryMap
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generateIr)
    val plansRef = Ref.unsafe[IO, RoutePlansLoadResult](plans)
    val app = DapHttpServerMain.routes(plansRef, mockDapClient(memory)).orNotFound

    val response =
      app.run(Request[IO](Method.GET, uri"/ComplexApi/gFighterInfo/spawnPos")).unsafeRunSync()
    assert(response.status == Status.Ok)
    val body = response.body.compile.toVector.unsafeRunSync().map(_.toChar).mkString
    val json = io.circe.parser.parse(body).toOption.get

    assert(json.hcursor.downField("member").as[String].toOption.contains("spawnPos"))
    assert(json.hcursor.downField("bytes").as[Int].toOption.contains(12))

    val decoded = json.hcursor.downField("decoded")
    assert(math.abs(decoded.downField("x").as[Double].toOption.get - 1.5) < 0.001)
    assert(math.abs(decoded.downField("y").as[Double].toOption.get - 2.0) < 0.001)
    assert(math.abs(decoded.downField("z").as[Double].toOption.get - (-3.5)) < 0.001)
  }

  test("non-pointer array sub-route reads individual element by index") {
    val memory = buildMemoryMap
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generateIr)
    val plansRef = Ref.unsafe[IO, RoutePlansLoadResult](plans)
    val app = DapHttpServerMain.routes(plansRef, mockDapClient(memory)).orNotFound

    val response =
      app
        .run(Request[IO](Method.GET, Uri.unsafeFromString("/ComplexApi/gFighterInfo/pad/0")))
        .unsafeRunSync()
    assert(response.status == Status.Ok)
    val body = response.body.compile.toVector.unsafeRunSync().map(_.toChar).mkString
    val json = io.circe.parser.parse(body).toOption.get

    assert(json.hcursor.downField("member").as[String].toOption.contains("pad"))
    assert(json.hcursor.downField("index").as[Int].toOption.contains(0))
    assert(json.hcursor.downField("decoded").as[Int].toOption.contains(0))
  }

  test("u64 value sub-route reads 8 bytes at offset") {
    val memory = buildMemoryMap
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(generateIr)
    val plansRef = Ref.unsafe[IO, RoutePlansLoadResult](plans)
    val app = DapHttpServerMain.routes(plansRef, mockDapClient(memory)).orNotFound

    val response =
      app.run(Request[IO](Method.GET, uri"/ComplexApi/gFighterInfo/uniqueId")).unsafeRunSync()
    assert(response.status == Status.Ok)
    val body = response.body.compile.toVector.unsafeRunSync().map(_.toChar).mkString
    val json = io.circe.parser.parse(body).toOption.get

    assert(json.hcursor.downField("member").as[String].toOption.contains("uniqueId"))
    assert(json.hcursor.downField("bytes").as[Int].toOption.contains(8))
    val uid = json.hcursor.downField("decoded").as[String].toOption.get
    assert(uid == "81985529216486895")
  }

  test("value sub-routes survive Smithy round-trip and serve identical data") {
    val ir = generateIr
    val smithyText = SmithyIrEmitter.emit(ir).fold(errors => fail(errors.mkString("\n")), identity)
    val model = Model
      .assembler()
      .addImport("src/main/smithy/dap-http-traits.smithy")
      .addUnparsedModel("complex.smithy", smithyText)
      .assemble()
      .unwrap()
    val roundTrippedIr = SmithyIrGenerator
      .generateFromModel(model)
      .fold(errors => fail(errors.mkString("\n")), identity)

    val memory = buildMemoryMap
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(roundTrippedIr)
    val plansRef = Ref.unsafe[IO, RoutePlansLoadResult](plans)
    val app = DapHttpServerMain.routes(plansRef, mockDapClient(memory)).orNotFound

    val weightResponse =
      app.run(Request[IO](Method.GET, uri"/ComplexApi/gFighterInfo/weight")).unsafeRunSync()
    assert(weightResponse.status == Status.Ok)
    val weightBody = weightResponse.body.compile.toVector.unsafeRunSync().map(_.toChar).mkString
    val weightJson = io.circe.parser.parse(weightBody).toOption.get
    assert(weightJson.hcursor.downField("decoded").as[Int].toOption.contains(100))

    val spawnResponse =
      app.run(Request[IO](Method.GET, uri"/ComplexApi/gFighterInfo/spawnPos")).unsafeRunSync()
    assert(spawnResponse.status == Status.Ok)
    val spawnBody = spawnResponse.body.compile.toVector.unsafeRunSync().map(_.toChar).mkString
    val spawnJson = io.circe.parser.parse(spawnBody).toOption.get
    val spawnDecoded = spawnJson.hcursor.downField("decoded")
    assert(math.abs(spawnDecoded.downField("x").as[Double].toOption.get - 1.5) < 0.001)
  }
}
