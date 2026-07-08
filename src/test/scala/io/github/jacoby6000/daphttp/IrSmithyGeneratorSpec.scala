package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.shapes.ShapeId

import java.nio.file.Paths

class IrSmithyGeneratorSpec extends AnyFunSuite {
  private val traitsPath = Paths
    .get("src/main/smithy/dap-http-traits.smithy")
    .toAbsolutePath
    .toString

  /** Round-trips IR services through the Smithy generator and IrExtractor. */
  private def roundTrip(services: List[IrService]): List[IrService] = {
    val smithyByNs = IrSmithyGenerator.generateSmithyFromIr(services)
    assert(smithyByNs.nonEmpty, "Generator produced no output")

    val assembler = Model.assembler().addImport(traitsPath)
    smithyByNs.foreach { case (ns, text) => assembler.addUnparsedModel(s"$ns.smithy", text) }
    val result = assembler.assemble()
    assert(!result.isBroken, result.getValidationEvents.toString)
    val model = result.unwrap()
    IrExtractor.buildIrFromModel(model).toOption.get
  }

  private def shapeId(ns: String, name: String): ShapeId =
    ShapeId.from(s"$ns#$name")

  private def memberId(ns: String, container: String, member: String): ShapeId =
    ShapeId.from(s"$ns#$container$$$member")

  private def noTraitMember(
      id: ShapeId,
      name: String,
      target: IrType,
      staticAddress: Option[Long] = None
  ): IrMember =
    IrMember(
      id = id,
      name = name,
      target = target,
      staticAddress = staticAddress,
      paddingRepeats = None,
      isPointer = false,
      isArray = false,
      arrayLength = None,
      endianOverride = None,
      primitiveOverride = None
    )

  test("generates valid Smithy for a service with primitive struct members") {
    val ns = "example.prim"
    val output = IrType.EnclosingStruct(
      id = shapeId(ns, "GetDataOutput"),
      members = List(
        noTraitMember(
          memberId(ns, "GetDataOutput", "flag"),
          "flag",
          IrType.Primitive(IrPrimitive.Bool),
          staticAddress = Some(0x1000L)
        ),
        noTraitMember(
          memberId(ns, "GetDataOutput", "count"),
          "count",
          IrType.Primitive(IrPrimitive.U32),
          staticAddress = Some(0x2000L)
        )
      ),
      declaredSizeBits = None
    )
    val services = List(
      IrService(
        name = "DataApi",
        wordSizeBits = Some(32),
        defaultEndian = IrEndian.Big,
        operations = List(IrOperation("GetData", "/DataApi/GetData", output))
      )
    )

    val result = roundTrip(services)
    assert(result.size == 1)
    val svc = result.head
    assert(svc.name == "DataApi")
    assert(svc.wordSizeBits.contains(32))
    assert(svc.defaultEndian == IrEndian.Big)
    assert(svc.operations.map(_.name) == List("GetData"))

    val outMembers = svc.operations.head.output.members
    val flag = outMembers.find(_.name == "flag").get
    val count = outMembers.find(_.name == "count").get
    assert(flag.staticAddress.contains(0x1000L))
    // Bool round-trips naturally via Boolean shape type
    assert(flag.target == IrType.Primitive(IrPrimitive.Bool))
    // U32 round-trips via @u32 primitiveOverride
    assert(count.primitiveOverride.contains(IrPrimitive.U32))
    assert(count.staticAddress.contains(0x2000L))
  }

  test("generates valid Smithy for a MemoryMappedStruct with @dapStruct") {
    val ns = "example.dap"
    val dapStruct: IrType.MemoryMappedStruct = IrType.MemoryMappedStruct(
      id = shapeId(ns, "Registers"),
      members = List(
        noTraitMember(memberId(ns, "Registers", "lo"), "lo", IrType.Primitive(IrPrimitive.S32)),
        IrMember(
          id = memberId(ns, "Registers", "hi"),
          name = "hi",
          target = IrType.Primitive(IrPrimitive.S32),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = Some(IrPrimitive.U16)
        )
      ),
      declaredSizeBits = Some(6)
    )
    val output: IrType.EnclosingStruct = IrType.EnclosingStruct(
      id = shapeId(ns, "GetRegistersOutput"),
      members = List(
        noTraitMember(
          memberId(ns, "GetRegistersOutput", "value"),
          "value",
          dapStruct,
          staticAddress = Some(0x8000L)
        )
      ),
      declaredSizeBits = None
    )
    val services = List(
      IrService(
        name = "RegApi",
        wordSizeBits = Some(64),
        defaultEndian = IrEndian.Little,
        operations = List(IrOperation("GetRegisters", "/RegApi/GetRegisters", output))
      )
    )

    val result = roundTrip(services)
    val svc = result.head
    assert(svc.defaultEndian == IrEndian.Little)
    assert(svc.wordSizeBits.contains(64))

    val outValue = svc.operations.head.output.members.find(_.name == "value").get
    assert(outValue.staticAddress.contains(0x8000L))

    val regs = outValue.target.asInstanceOf[IrType.MemoryMappedStruct]
    assert(regs.declaredSizeBits.contains(6))
    assert(regs.members.find(_.name == "lo").get.primitiveOverride.isEmpty)
    assert(regs.members.find(_.name == "hi").get.primitiveOverride.contains(IrPrimitive.U16))
  }

  test("generates valid Smithy for a Bitmask struct") {
    val ns = "example.bitmask"
    val bitmask: IrType.Bitmask = IrType.Bitmask(
      id = shapeId(ns, "StatusFlags"),
      members = List(
        noTraitMember(
          memberId(ns, "StatusFlags", "ready"),
          "ready",
          IrType.Primitive(IrPrimitive.Bool)
        ),
        noTraitMember(
          memberId(ns, "StatusFlags", "error"),
          "error",
          IrType.Primitive(IrPrimitive.Bool)
        )
      ),
      declaredSizeBits = Some(8)
    )
    val output: IrType.EnclosingStruct = IrType.EnclosingStruct(
      id = shapeId(ns, "GetStatusOutput"),
      members = List(
        noTraitMember(
          memberId(ns, "GetStatusOutput", "flags"),
          "flags",
          bitmask,
          staticAddress = Some(0xff00L)
        )
      ),
      declaredSizeBits = None
    )
    val services = List(
      IrService(
        name = "StatusApi",
        wordSizeBits = Some(32),
        defaultEndian = IrEndian.Big,
        operations = List(IrOperation("GetStatus", "/StatusApi/GetStatus", output))
      )
    )

    val result = roundTrip(services)
    val flags = result.head.operations.head.output.members.find(_.name == "flags").get
    val mask = flags.target.asInstanceOf[IrType.Bitmask]
    assert(mask.declaredSizeBits.contains(8))
    assert(mask.members.map(_.name) == List("ready", "error"))
    assert(mask.members.forall(_.target == IrType.Primitive(IrPrimitive.Bool)))
  }

  test("generates valid Smithy for members with pointer/array/endian traits") {
    val ns = "example.traits"
    val inner: IrType.MemoryMappedStruct = IrType.MemoryMappedStruct(
      id = shapeId(ns, "Inner"),
      members = List(
        noTraitMember(memberId(ns, "Inner", "x"), "x", IrType.Primitive(IrPrimitive.S32))
      ),
      declaredSizeBits = None
    )
    val innerList = IrType.ListType(
      id = shapeId(ns, "InnerList"),
      element = inner,
      bytesAlias = false,
      bitsAlias = false
    )
    val output: IrType.EnclosingStruct = IrType.EnclosingStruct(
      id = shapeId(ns, "GetThingOutput"),
      members = List(
        IrMember(
          id = memberId(ns, "GetThingOutput", "ptr"),
          name = "ptr",
          target = IrType.Primitive(IrPrimitive.LongWord),
          staticAddress = Some(0x3000L),
          paddingRepeats = None,
          isPointer = true,
          isArray = false,
          arrayLength = None,
          endianOverride = Some(IrEndian.Little),
          primitiveOverride = None
        ),
        IrMember(
          id = memberId(ns, "GetThingOutput", "items"),
          name = "items",
          target = innerList,
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = true,
          arrayLength = Some(4),
          endianOverride = None,
          primitiveOverride = None
        )
      ),
      declaredSizeBits = None
    )
    val services = List(
      IrService(
        name = "ThingApi",
        wordSizeBits = Some(32),
        defaultEndian = IrEndian.Big,
        operations = List(IrOperation("GetThing", "/ThingApi/GetThing", output))
      )
    )

    val result = roundTrip(services)
    val outMembers = result.head.operations.head.output.members
    val ptr = outMembers.find(_.name == "ptr").get
    val items = outMembers.find(_.name == "items").get

    assert(ptr.isPointer)
    assert(ptr.endianOverride.contains(IrEndian.Little))
    assert(ptr.staticAddress.contains(0x3000L))
    assert(items.isArray)
    assert(items.arrayLength.contains(4))
  }

  test("generates valid Smithy for union and map types") {
    val ns = "example.complex"
    val union: IrType.Union = IrType.Union(
      id = shapeId(ns, "NumberChoice"),
      members = List(
        noTraitMember(
          memberId(ns, "NumberChoice", "intVal"),
          "intVal",
          IrType.Primitive(IrPrimitive.S32)
        )
      )
    )
    val mapType = IrType.MapType(
      id = shapeId(ns, "StringToInt"),
      key = IrType.Primitive(IrPrimitive.S32),
      value = IrType.Primitive(IrPrimitive.S32)
    )
    val output: IrType.EnclosingStruct = IrType.EnclosingStruct(
      id = shapeId(ns, "GetComplexOutput"),
      members = List(
        noTraitMember(
          memberId(ns, "GetComplexOutput", "choice"),
          "choice",
          union
        ),
        noTraitMember(
          memberId(ns, "GetComplexOutput", "lookup"),
          "lookup",
          mapType
        )
      ),
      declaredSizeBits = None
    )
    val services = List(
      IrService(
        name = "ComplexApi",
        wordSizeBits = Some(32),
        defaultEndian = IrEndian.Big,
        operations = List(IrOperation("GetComplex", "/ComplexApi/GetComplex", output))
      )
    )

    val result = roundTrip(services)
    val members = result.head.operations.head.output.members
    assert(members.find(_.name == "choice").get.target.isInstanceOf[IrType.Union])
    assert(members.find(_.name == "lookup").get.target.isInstanceOf[IrType.MapType])
  }

  test("generates valid Smithy from doldecomp fixture IR") {
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

    val smithyByNs = IrSmithyGenerator.generateSmithyFromIr(irServices)
    assert(smithyByNs.size == 1)

    val assembler = Model.assembler().addImport(traitsPath)
    smithyByNs.foreach { case (ns, text) => assembler.addUnparsedModel(s"$ns.smithy", text) }
    val result = assembler.assemble()
    assert(!result.isBroken, result.getValidationEvents.toString)

    val model = result.unwrap()
    val reExtracted = IrExtractor.buildIrFromModel(model).toOption.get
    assert(reExtracted.size == 1)
    val svc = reExtracted.head
    assert(svc.name == "MeleeApi")
    assert(svc.wordSizeBits.contains(32))
    assert(svc.defaultEndian == IrEndian.Big)
    assert(svc.operations.map(_.name) == List("GetGPlayerState"))

    val playerStruct = svc.operations.head.output.members.head.target.asInstanceOf[IrType.Struct]
    assert(playerStruct.members.exists(_.name == "score"))
  }
}
