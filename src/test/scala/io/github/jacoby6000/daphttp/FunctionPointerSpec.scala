package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.BitVector
import software.amazon.smithy.model.Model

import java.nio.file.Paths

class FunctionPointerSpec extends AnyFunSuite {
  private val meleeFixtureRoot =
    Paths.get("src/test/resources/doldecomp-fixture-melee-style")

  private val fnptrFixtureRoot =
    Paths.get("src/test/resources/doldecomp-fixture-fnptr")

  private val meleeIr = DoldecompIrGenerator
    .generateFromPaths(
      symbolsPath = meleeFixtureRoot.resolve("symbols.txt"),
      headerRoots = List(meleeFixtureRoot),
      namespace = "example.melee",
      serviceName = "MeleeApi",
      wordSizeBits = 32
    )
    .toOption
    .get

  private val fnptrIr = DoldecompIrGenerator
    .generateFromPaths(
      symbolsPath = fnptrFixtureRoot.resolve("symbols.txt"),
      headerRoots = List(fnptrFixtureRoot),
      namespace = "example.fnptr",
      serviceName = "FnPtrApi",
      wordSizeBits = 32
    )
    .toOption
    .get

  test("function pointer struct members are detected as IrType.FunctionPointer") {
    val operation = meleeIr.services.head.operations.head
    val valueMember = operation.output.members.head
    val listType = valueMember.target.asInstanceOf[IrType.ListType]
    val struct = listType.element.asInstanceOf[IrType.MemoryMappedStruct]
    val memberNames = struct.members.map(_.name)
    assert(memberNames.contains("prep"))
    assert(memberNames.contains("decide"))

    val prep = struct.members.find(_.name == "prep").get
    val decide = struct.members.find(_.name == "decide").get

    assert(prep.target.isInstanceOf[IrType.FunctionPointer])
    assert(decide.target.isInstanceOf[IrType.FunctionPointer])
    assert(prep.isPointer)
    assert(decide.isPointer)
  }

  test("function pointer signature is extracted correctly") {
    val operation = meleeIr.services.head.operations.head
    val valueMember = operation.output.members.head
    val listType = valueMember.target.asInstanceOf[IrType.ListType]
    val struct = listType.element.asInstanceOf[IrType.MemoryMappedStruct]

    val prep = struct.members.find(_.name == "prep").get
    val fp = prep.target.asInstanceOf[IrType.FunctionPointer]
    assert(fp.name == "Prep")
    assert(fp.returnType == "void")
    assert(fp.params.length == 1)
    assert(fp.params.head.typeName == "GameScene")
    assert(fp.params.head.name == "arg0")

    val decide = struct.members.find(_.name == "decide").get
    val fp2 = decide.target.asInstanceOf[IrType.FunctionPointer]
    assert(fp2.name == "Decide")
    assert(fp2.returnType == "void")
    assert(fp2.params.length == 1)
  }

  test("function pointer void* members are NOT confused with function pointers") {
    val operation = meleeIr.services.head.operations.head
    val valueMember = operation.output.members.head
    val listType = valueMember.target.asInstanceOf[IrType.ListType]
    val struct = listType.element.asInstanceOf[IrType.MemoryMappedStruct]

    val nestedInfo = struct.members.find(_.name == "info").get
    val infoStruct = nestedInfo.target.asInstanceOf[IrType.MemoryMappedStruct]
    val loadData = infoStruct.members.find(_.name == "loadData").get
    val leaveData = infoStruct.members.find(_.name == "leaveData").get

    assert(!loadData.target.isInstanceOf[IrType.FunctionPointer])
    assert(!leaveData.target.isInstanceOf[IrType.FunctionPointer])
    assert(loadData.isPointer)
    assert(leaveData.isPointer)
  }

  test("function pointer with named parameter and return type") {
    val operation = fnptrIr.services.head.operations.head
    val valueMember = operation.output.members.head
    val struct = valueMember.target.asInstanceOf[IrType.MemoryMappedStruct]

    val onInit = struct.members.find(_.name == "onInit").get
    val fpInit = onInit.target.asInstanceOf[IrType.FunctionPointer]
    assert(fpInit.name == "OnInit")
    assert(fpInit.returnType == "void")
    assert(fpInit.params.isEmpty)

    val onUpdate = struct.members.find(_.name == "onUpdate").get
    val fpUpdate = onUpdate.target.asInstanceOf[IrType.FunctionPointer]
    assert(fpUpdate.name == "OnUpdate")
    assert(fpUpdate.returnType == "s32")
    assert(fpUpdate.params.length == 1)
    assert(fpUpdate.params.head.typeName == "s32")
    assert(fpUpdate.params.head.name == "frame")

    val onDestroy = struct.members.find(_.name == "onDestroy").get
    val fpDestroy = onDestroy.target.asInstanceOf[IrType.FunctionPointer]
    assert(fpDestroy.name == "OnDestroy")
    assert(fpDestroy.returnType == "void")
    assert(fpDestroy.params.isEmpty)
  }

  test("function pointer codec decodes as formatted string") {
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(fnptrIr.services)
    assert(plans.errors.isEmpty)

    val route = plans.routes("/api/FnPtrApi/g_CallbackTable")
    assert(route.reads.nonEmpty)
    assert(route.reads.head.decodeCodec.nonEmpty)

    val codec = route.reads.head.decodeCodec.get
    val onInitAddr = 0x80001234L
    val onUpdateAddr = 0x80005678L
    val onDestroyAddr = 0x80009abcL
    val bytes = buildCallbackTableBytes(onInitAddr, onUpdateAddr, onDestroyAddr)
    val result = codec.decode(BitVector(bytes))
    assert(result.isSuccessful, s"decode failed: ${result.fold(_.message, _ => "")}")

    val decoded = result.require.value
    val obj = decoded.asObject.get
    val onInitStr = obj("onInit").get.asString.get
    assert(onInitStr == "<function OnInit() @ 0x80001234>")

    val onUpdateStr = obj("onUpdate").get.asString.get
    assert(onUpdateStr == "<function OnUpdate(s32 frame) @ 0x80005678>")

    val onDestroyStr = obj("onDestroy").get.asString.get
    assert(onDestroyStr == "<function OnDestroy() @ 0x80009abc>")
  }

  test("function pointer members get ValueSubRoutes not PointerSubRoutes") {
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(fnptrIr.services)
    assert(plans.errors.isEmpty)

    val route = plans.routes("/api/FnPtrApi/g_CallbackTable")
    val onInitSubRoute = route.memberSubRoutes.find(_.memberName == "onInit")
    assert(onInitSubRoute.isDefined)
    assert(onInitSubRoute.get.isInstanceOf[MemberSubRoute.ValueSubRoute])

    val onDestroySubRoute = route.memberSubRoutes.find(_.memberName == "onDestroy")
    assert(onDestroySubRoute.isDefined)
    assert(onDestroySubRoute.get.isInstanceOf[MemberSubRoute.ValueSubRoute])
  }

  test("function pointer ValueSubRoute has correct element size") {
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(fnptrIr.services)
    val route = plans.routes("/api/FnPtrApi/g_CallbackTable")
    val onInitSubRoute = route.memberSubRoutes
      .find(_.memberName == "onInit")
      .get
      .asInstanceOf[MemberSubRoute.ValueSubRoute]

    assert(onInitSubRoute.elementSizeBytes.contains(4))
    assert(onInitSubRoute.decodeCodec.isDefined)
  }

  test("function pointer ValueSubRoute codec decodes address correctly") {
    val plans = HttpRouteIrEmitter.emitRoutePlansFromIr(fnptrIr.services)
    val route = plans.routes("/api/FnPtrApi/g_CallbackTable")
    val onInitSubRoute = route.memberSubRoutes
      .find(_.memberName == "onInit")
      .get
      .asInstanceOf[MemberSubRoute.ValueSubRoute]

    val bytes = longToBigEndian(0x80001234L, 4)
    val result = onInitSubRoute.decodeCodec.get.decode(BitVector(bytes))
    assert(result.isSuccessful)
    val decoded = result.require.value
    assert(decoded.asString.get == "<function OnInit() @ 0x80001234>")
  }

  test("function pointer info survives Smithy round-trip") {
    val smithyText =
      SmithyIrEmitter.emit(fnptrIr.services).fold(errors => fail(errors.mkString("\n")), identity)
    val model = Model
      .assembler()
      .addImport("src/main/smithy/dap-http-traits.smithy")
      .addUnparsedModel("fnptr.smithy", smithyText)
      .assemble()
      .unwrap()
    val roundTripped =
      SmithyIrGenerator
        .generateFromModel(model)
        .fold(errors => fail(errors.mkString("\n")), identity)

    IrEquivalence.assertEquivalent(fnptrIr.services, roundTripped)

    val service = roundTripped.head
    val operation = service.operations.head
    val valueMember = operation.output.members.head
    val struct = valueMember.target.asInstanceOf[IrType.MemoryMappedStruct]

    val onInit = struct.members.find(_.name == "onInit").get
    assert(onInit.target.isInstanceOf[IrType.FunctionPointer])
    val fp = onInit.target.asInstanceOf[IrType.FunctionPointer]
    assert(fp.name == "OnInit")
    assert(fp.returnType == "void")
    assert(fp.params.isEmpty)

    val onUpdate = struct.members.find(_.name == "onUpdate").get
    val fpUpdate = onUpdate.target.asInstanceOf[IrType.FunctionPointer]
    assert(fpUpdate.name == "OnUpdate")
    assert(fpUpdate.returnType == "s32")
    assert(fpUpdate.params.length == 1)
    assert(fpUpdate.params.head.typeName == "s32")
    assert(fpUpdate.params.head.name == "frame")
  }

  private def buildCallbackTableBytes(
      onInitAddr: Long,
      onUpdateAddr: Long,
      onDestroyAddr: Long
  ): Array[Byte] = {
    val bytes = new Array[Byte](13)
    bytes(0) = 0x01
    val initBytes = longToBigEndian(onInitAddr, 4)
    System.arraycopy(initBytes, 0, bytes, 1, 4)
    val updateBytes = longToBigEndian(onUpdateAddr, 4)
    System.arraycopy(updateBytes, 0, bytes, 5, 4)
    val destroyBytes = longToBigEndian(onDestroyAddr, 4)
    System.arraycopy(destroyBytes, 0, bytes, 9, 4)
    bytes
  }

  private def longToBigEndian(value: Long, length: Int): Array[Byte] = {
    val result = new Array[Byte](length)
    var v = value
    for (i <- (length - 1) to 0 by -1) {
      result(i) = (v & 0xff).toByte
      v = v >> 8
    }
    result
  }
}
