package io.github.jacoby6000.daphttp

import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.BitVector
import scodec.codecs._
import software.amazon.smithy.model.shapes.ShapeId

class WatchPathResolverSpec extends AnyFunSuite {
  private val littleU32 = uint32L.xmap[Json](
    v => Json.fromLong(v),
    j => j.asNumber.flatMap(_.toLong).getOrElse(0L)
  )

  test("resolves root read plan for watching") {
    val plan = RoutePlan(
      path = "/api/Demo/gValue",
      reads = List(
        ReadPlan(
          path = "/api/Demo/gValue",
          address = 0x80000000L,
          sizeBytes = 4,
          decodeType = Some(IrType.Primitive(IrPrimitive.U32)),
          endian = IrEndian.Little,
          wordSizeBits = Some(32),
          decodeCodec = Some(littleU32),
          cStringPointer = false
        )
      )
    )
    val resolved = WatchPathResolver.resolve("/api/Demo/gValue", Map(plan.path -> plan))
    assert(resolved.map(_.address).contains(0x80000000L))
    assert(resolved.map(_.count).contains(4))
    assert(resolved.map(_.sourceOffsetInWatch).contains(0))
    assert(resolved.map(_.overlayFields).contains(Nil))
  }

  test("resolves value member subroute with index stride") {
    val sub = MemberSubRoute.ValueSubRoute(
      memberName = "counts",
      baseAddress = 0x80001000L,
      memberOffsetBytes = 4,
      isArray = true,
      arrayLength = Some(8),
      wordSizeBits = 32,
      endian = IrEndian.Big,
      valueType = Some(IrType.Primitive(IrPrimitive.U32)),
      elementSizeBytes = Some(4),
      elementStrideBytes = Some(4),
      decodeCodec = Some(uint32.xmap[Json](v => Json.fromLong(v), _ => 0L))
    )
    val plan = RoutePlan(
      path = "/api/Demo/gStats",
      reads = List(
        ReadPlan(
          path = "/api/Demo/gStats",
          address = 0x80001000L,
          sizeBytes = 36,
          decodeType = None,
          endian = IrEndian.Big,
          wordSizeBits = Some(32),
          decodeCodec = None,
          cStringPointer = false
        )
      ),
      memberSubRoutes = List(sub)
    )
    val resolved = WatchPathResolver.resolve("/api/Demo/gStats/counts/2", Map(plan.path -> plan))
    assert(resolved.map(_.address).contains(0x80001000L + 4 + 8))
    assert(resolved.map(_.count).contains(4))
    assert(resolved.map(_.parentPath).contains("/api/Demo/gStats"))
  }

  test("resolves root array element watch path") {
    val sub = MemberSubRoute.ValueSubRoute(
      memberName = MemberSubRoute.RootArrayMemberName,
      baseAddress = 0x80453080L,
      memberOffsetBytes = 0,
      isArray = true,
      arrayLength = Some(6),
      wordSizeBits = 32,
      endian = IrEndian.Big,
      valueType = Some(IrType.Primitive(IrPrimitive.U32)),
      elementSizeBytes = Some(4),
      elementStrideBytes = Some(4),
      decodeCodec = Some(uint32.xmap[Json](v => Json.fromLong(v), _ => 0L))
    )
    val plan = RoutePlan(
      path = "/api/MasterHand/player_slots",
      reads = List(
        ReadPlan(
          path = "/api/MasterHand/player_slots",
          address = 0x80453080L,
          sizeBytes = 24,
          decodeType = None,
          endian = IrEndian.Big,
          wordSizeBits = Some(32),
          decodeCodec = None,
          cStringPointer = false,
          arrayLength = Some(6)
        )
      ),
      memberSubRoutes = List(sub)
    )
    val resolved =
      WatchPathResolver.resolve("/api/MasterHand/player_slots/2", Map(plan.path -> plan))
    assert(resolved.map(_.address).contains(0x80453080L + 8))
    assert(resolved.map(_.count).contains(4))
    assert(resolved.map(_.parentPath).contains("/api/MasterHand/player_slots"))
  }

  test("resolves nested field under root array element") {
    import software.amazon.smithy.model.shapes.ShapeId
    def sid(name: String): ShapeId = ShapeId.from(s"example#$name")
    val slot = IrType.MemoryMappedStruct(
      id = sid("StaticPlayer"),
      members = List(
        IrMember(
          id = sid("StaticPlayer_x"),
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
        ),
        IrMember(
          id = sid("StaticPlayer_y"),
          name = "y",
          target = IrType.Primitive(IrPrimitive.U16),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = Some(IrPrimitive.U16),
          offsetBytes = Some(4)
        )
      ),
      declaredSizeBits = Some(6)
    )
    val sub = MemberSubRoute.ValueSubRoute(
      memberName = MemberSubRoute.RootArrayMemberName,
      baseAddress = 0x80453080L,
      memberOffsetBytes = 0,
      isArray = true,
      arrayLength = Some(6),
      wordSizeBits = 32,
      endian = IrEndian.Big,
      valueType = Some(slot),
      elementSizeBytes = Some(6),
      elementStrideBytes = Some(8),
      decodeCodec = HttpRouteIrEmitter.codecForType(slot, IrEndian.Big, Some(32))
    )
    val plan = RoutePlan(
      path = "/api/MasterHand/player_slots",
      reads = List(
        ReadPlan(
          path = "/api/MasterHand/player_slots",
          address = 0x80453080L,
          sizeBytes = 48,
          decodeType = Some(
            IrType.ListType(sid("Slots"), slot, bytesAlias = false, bitsAlias = false)
          ),
          endian = IrEndian.Big,
          wordSizeBits = Some(32),
          decodeCodec = None,
          cStringPointer = false,
          arrayLength = Some(6)
        )
      ),
      memberSubRoutes = List(sub)
    )
    val resolved =
      WatchPathResolver.resolve("/api/MasterHand/player_slots/2/y", Map(plan.path -> plan))
    assert(resolved.map(_.address).contains(0x80453080L + 2 * 8 + 4))
    assert(resolved.map(_.count).contains(2))
  }

  test("parses dolphin_memoryChanged body shape via framed session helpers") {
    // Decode path used by RealtimeWatchService: base64 → codec → Json
    val bytes = Array[Byte](0x01, 0x02, 0x03, 0x04)
    val data = java.util.Base64.getEncoder.encodeToString(bytes)
    val event = MemoryChangedEvent(3, 0x1000L, 4, data)
    val decoded = littleU32.decode(BitVector(java.util.Base64.getDecoder.decode(event.dataBase64)))
    assert(decoded.isSuccessful)
    assert(event.watchId == 3)
  }

  test("expands member watch to cover overlapping overlay fields") {
    val structId = ShapeId.from("demo#Stats")
    val sourceStruct = IrType.MemoryMappedStruct(
      id = structId,
      members = List(
        IrMember(
          id = ShapeId.from("demo#Stats_a"),
          name = "a",
          target = IrType.Primitive(IrPrimitive.U32),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = None,
          offsetBytes = Some(0)
        ),
        IrMember(
          id = ShapeId.from("demo#Stats_b"),
          name = "b",
          target = IrType.Primitive(IrPrimitive.U32),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = None,
          offsetBytes = Some(4)
        )
      ),
      declaredSizeBits = Some(8)
    )
    val sub = MemberSubRoute.ValueSubRoute(
      memberName = "a",
      baseAddress = 0x80001000L,
      memberOffsetBytes = 0,
      isArray = false,
      arrayLength = None,
      wordSizeBits = 32,
      endian = IrEndian.Big,
      valueType = Some(IrType.Primitive(IrPrimitive.U32)),
      elementSizeBytes = Some(4),
      elementStrideBytes = None,
      decodeCodec = Some(uint32.xmap[Json](v => Json.fromLong(v), _ => 0L))
    )
    val plan = RoutePlan(
      path = "/api/Demo/gStats",
      reads = List(
        ReadPlan(
          path = "/api/Demo/gStats",
          address = 0x80001000L,
          sizeBytes = 8,
          decodeType = Some(sourceStruct),
          endian = IrEndian.Big,
          wordSizeBits = Some(32),
          decodeCodec = None,
          cStringPointer = false
        )
      ),
      memberSubRoutes = List(sub)
    )
    val document = TypeOverlayDocument(
      structs = Map(
        structId.toString -> OverlayStructDef(
          List(OverlayMember("wide", "u64"))
        )
      )
    )
    val engine = OverlayEngine.fromServices(
      document,
      List(
        IrService(
          name = "Demo",
          wordSizeBits = Some(32),
          defaultEndian = IrEndian.Big,
          operations = Nil
        )
      )
    )
    val resolved =
      WatchPathResolver.resolve("/api/Demo/gStats/a", Map(plan.path -> plan), engine)
    assert(resolved.isRight, resolved)
    val target = resolved.toOption.get
    assert(target.count >= 8)
    assert(target.overlayFields.map(_.segments).contains(List("wide")))
    assert(target.sourceOffsetInWatch == 0)
  }

  test("expands nested root-array field watch with nested overlay segments") {
    import software.amazon.smithy.model.shapes.ShapeId
    def sid(name: String): ShapeId = ShapeId.from(s"example#$name")
    val slotId = sid("StaticPlayer")
    val slot = IrType.MemoryMappedStruct(
      id = slotId,
      members = List(
        IrMember(
          id = sid("StaticPlayer_x"),
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
        ),
        IrMember(
          id = sid("StaticPlayer_y"),
          name = "y",
          target = IrType.Primitive(IrPrimitive.U16),
          staticAddress = None,
          paddingRepeats = None,
          isPointer = false,
          isArray = false,
          arrayLength = None,
          endianOverride = None,
          primitiveOverride = Some(IrPrimitive.U16),
          offsetBytes = Some(4)
        )
      ),
      declaredSizeBits = Some(6)
    )
    val sub = MemberSubRoute.ValueSubRoute(
      memberName = MemberSubRoute.RootArrayMemberName,
      baseAddress = 0x80453080L,
      memberOffsetBytes = 0,
      isArray = true,
      arrayLength = Some(6),
      wordSizeBits = 32,
      endian = IrEndian.Big,
      valueType = Some(slot),
      elementSizeBytes = Some(6),
      elementStrideBytes = Some(8),
      decodeCodec = HttpRouteIrEmitter.codecForType(slot, IrEndian.Big, Some(32))
    )
    val plan = RoutePlan(
      path = "/api/MasterHand/player_slots",
      reads = List(
        ReadPlan(
          path = "/api/MasterHand/player_slots",
          address = 0x80453080L,
          sizeBytes = 48,
          decodeType = Some(
            IrType.ListType(sid("Slots"), slot, bytesAlias = false, bitsAlias = false)
          ),
          endian = IrEndian.Big,
          wordSizeBits = Some(32),
          decodeCodec = None,
          cStringPointer = false,
          arrayLength = Some(6)
        )
      ),
      memberSubRoutes = List(sub)
    )
    val document = TypeOverlayDocument(
      structs = Map(
        slotId.toString -> OverlayStructDef(
          List(OverlayMember("xy", "u64"))
        )
      )
    )
    val engine = OverlayEngine.fromServices(
      document,
      List(
        IrService(
          name = "MasterHand",
          wordSizeBits = Some(32),
          defaultEndian = IrEndian.Big,
          operations = Nil
        )
      )
    )
    val resolved =
      WatchPathResolver.resolve(
        "/api/MasterHand/player_slots/2/x",
        Map(plan.path -> plan),
        engine
      )
    assert(resolved.isRight, resolved)
    val target = resolved.toOption.get
    assert(target.address == 0x80453080L + 2 * 8)
    assert(target.count >= 8)
    assert(target.overlayFields.map(_.segments).contains(List("2", "xy")))
  }
}
