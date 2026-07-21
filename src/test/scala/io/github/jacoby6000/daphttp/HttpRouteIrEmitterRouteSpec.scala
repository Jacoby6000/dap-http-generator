package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite
import software.amazon.smithy.model.shapes.ShapeId

class HttpRouteIrEmitterRouteSpec extends AnyFunSuite {
  private def id(value: String): ShapeId = ShapeId.from(s"example#$value")

  test("compiles IR into route reads for dap-backed members") {
    val registerStruct = IrType.MemoryMappedStruct(
      id = id("RegisterBlock"),
      members = List(
        IrMember(
          id = id("RegisterBlock_lo"),
          name = "lo",
          target = IrType.Primitive(IrPrimitive.U16),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = Some(IrPrimitive.U16)
        ),
        IrMember(
          id = id("RegisterBlock_hi"),
          name = "hi",
          target = IrType.Primitive(IrPrimitive.U16),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = Some(IrPrimitive.U16)
        )
      ),
      declaredSizeBits = Some(4)
    )

    val output = IrType.EnclosingStruct(
      id = id("GetSnapshotOutput"),
      members = List(
        IrMember(
          id = id("GetSnapshotOutput_registers"),
          name = "registers",
          target = registerStruct,
          staticAddress = Some(0x1000L),
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = None
        )
      ),
      declaredSizeBits = None
    )

    val plans = HttpRouteIrEmitter
      .emitRoutePlansFromIr(
        List(
          IrService(
            name = "Api",
            wordSizeBits = Some(32),
            defaultEndian = IrEndian.Big,
            operations = List(
              IrOperation(name = "GetSnapshot", routePath = "/api/Api/GetSnapshot", output = output)
            )
          )
        )
      )
      .routes

    val read = plans("/api/Api/GetSnapshot").reads.head
    assert(read.path == "/api/Api/GetSnapshot.registers")
    assert(read.address == 0x1000L)
    assert(read.sizeBytes == 4)
    assert(read.decodeCodec.nonEmpty)
  }

  test("compiles bitmask size from bits into route read bytes") {
    val flags = IrType.Bitmask(
      id = id("Flags"),
      members = List(
        IrMember(
          id = id("Flags_ready"),
          name = "ready",
          target = IrType.Primitive(IrPrimitive.Bool),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = None
        ),
        IrMember(
          id = id("Flags_error"),
          name = "error",
          target = IrType.Primitive(IrPrimitive.Bool),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = None
        )
      ),
      declaredSizeBits = Some(10)
    )

    val output = IrType.EnclosingStruct(
      id = id("GetFlagsOutput"),
      members = List(
        IrMember(
          id = id("GetFlagsOutput_flags"),
          name = "flags",
          target = flags,
          staticAddress = Some(0x2000L),
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = None
        )
      ),
      declaredSizeBits = None
    )

    val plans = HttpRouteIrEmitter
      .emitRoutePlansFromIr(
        List(
          IrService(
            name = "Api",
            wordSizeBits = Some(32),
            defaultEndian = IrEndian.Big,
            operations =
              List(IrOperation(name = "GetFlags", routePath = "/api/Api/GetFlags", output = output))
          )
        )
      )
      .routes

    val read = plans("/api/Api/GetFlags").reads.head
    assert(read.sizeBytes == 2)
  }

  test("member offsets do not stall when a member size cannot be determined") {
    import io.circe.Json

    val cell = IrType.MemoryMappedStruct(
      id = id("Cell"),
      members = List(
        IrMember(
          id = id("Cell_v"),
          name = "v",
          target = IrType.Primitive(IrPrimitive.U32),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = Some(IrPrimitive.U32),
          offsetBytes = Some(0)
        )
      ),
      declaredSizeBits = Some(4)
    )
    val opaque = IrType.MapType(
      id = id("OpaqueMap"),
      key = IrType.Primitive(IrPrimitive.U32),
      value = IrType.Primitive(IrPrimitive.U32)
    )
    val struct = IrType.MemoryMappedStruct(
      id = id("Mixed"),
      members = List(
        IrMember(
          id = id("Mixed_a"),
          name = "a",
          target = cell,
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = None
        ),
        IrMember(
          id = id("Mixed_opaque"),
          name = "opaque",
          target = opaque,
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = None,
          offsetBytes = Some(0x20)
        ),
        IrMember(
          id = id("Mixed_b"),
          name = "b",
          target = cell,
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = None
        )
      ),
      declaredSizeBits = Some(0x28)
    )

    val decoded = Json.obj(
      "a" -> Json.obj("v" -> Json.fromLong(1L)),
      "opaque" -> Json.obj("0" -> Json.fromLong(0L)),
      "b" -> Json.obj("v" -> Json.fromLong(2L))
    )
    val annotated =
      HttpRouteIrEmitter.annotateDecodedAddresses(struct, decoded, 0x1000L, Some(32))

    assert(
      annotated.hcursor.downField("a").downField("_address").as[String].toOption.contains("0x1000")
    )
    // Sequential member after an unsized explicit-offset field must keep the packed cursor
    // (0x1004), not collide with the opaque member at 0x1020.
    assert(
      annotated.hcursor.downField("b").downField("_address").as[String].toOption.contains("0x1004")
    )
  }

  test("root-level array reads expose indexed element subroutes") {
    val slot = IrType.MemoryMappedStruct(
      id = id("StaticPlayer"),
      members = List(
        IrMember(
          id = id("StaticPlayer_x"),
          name = "x",
          target = IrType.Primitive(IrPrimitive.U32),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = Some(IrPrimitive.U32),
          offsetBytes = Some(0)
        )
      ),
      declaredSizeBits = Some(4)
    )
    val list = IrType.ListType(
      id = id("PlayerSlotsArray"),
      element = slot,
      bytesAlias = false,
      bitsAlias = false
    )
    val output = IrType.EnclosingStruct(
      id = id("PlayerSlotsOutput"),
      members = List(
        IrMember(
          id = id("PlayerSlotsOutput_value"),
          name = "value",
          target = list,
          staticAddress = Some(0x80453080L),
          paddingRepeats = None,
          isPointer = false,
          isArray = true,
          arrayLength = Some(6),
          endianOverride = None,
          primitiveOverride = None,
          readSizeBytes = Some(24)
        )
      ),
      declaredSizeBits = None
    )
    val result = HttpRouteIrEmitter.emitRoutePlansFromIr(
      List(
        IrService(
          name = "MasterHand",
          wordSizeBits = Some(32),
          defaultEndian = IrEndian.Big,
          operations = List(
            IrOperation(
              name = "GetPlayerSlots",
              routePath = "/MasterHand/player_slots",
              output = output
            )
          )
        )
      )
    )
    assert(result.errors.isEmpty)
    val plan = result.routes("/api/MasterHand/player_slots")
    val rootArray = plan.memberSubRoutes.find(_.memberName == MemberSubRoute.RootArrayMemberName)
    assert(rootArray.isDefined)
    assert(rootArray.get.isArray)
    assert(rootArray.get.arrayLength.contains(6))

    val tree = RouteTree.fromPlans(result.routes)
    val root = tree.find(_.path == "/api/MasterHand/player_slots").get
    val elementPaths = root.children.flatMap(_.children).map(_.path).toSet
    assert(elementPaths.contains("/api/MasterHand/player_slots/0"))
    assert(elementPaths.contains("/api/MasterHand/player_slots/5"))
    assert(root.children.flatMap(_.children).forall(_.fetchable))

    val matched =
      DapHttpServerMain.matchMemberSubRoutePublic("/api/MasterHand/player_slots/3", result.routes)
    assert(matched.isDefined)
    assert(matched.get._3.contains(3))

    val nested = DapHttpServerMain.resolveMemberPathPublic(
      "/api/MasterHand/player_slots/3/x",
      result.routes
    )
    assert(nested.isDefined)
    assert(nested.get.address == 0x80453080L + 3 * 4)
    assert(nested.get.sizeBytes == 4)
  }
}
