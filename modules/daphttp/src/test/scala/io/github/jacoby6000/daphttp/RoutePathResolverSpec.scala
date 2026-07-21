package io.github.jacoby6000.daphttp

import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite
import scodec.codecs._

/** Characterizes HTTP vs watch path resolution, especially pointer members. */
class RoutePathResolverSpec extends AnyFunSuite {
  private val bigU32 = uint32.xmap[Json](
    v => Json.fromLong(v),
    j => j.asNumber.flatMap(_.toLong).getOrElse(0L)
  )

  private val pointerSub = MemberSubRoute.PointerSubRoute(
    memberName = "next",
    baseAddress = 0x80001000L,
    memberOffsetBytes = 4,
    isArray = false,
    arrayLength = None,
    wordSizeBits = 32,
    endian = IrEndian.Big,
    pointeeType = Some(IrType.Primitive(IrPrimitive.U32)),
    pointeeSizeBytes = Some(4),
    pointeeDecodeCodec = Some(bigU32),
    followCString = false
  )

  private val valueSub = MemberSubRoute.ValueSubRoute(
    memberName = "count",
    baseAddress = 0x80001000L,
    memberOffsetBytes = 0,
    isArray = false,
    arrayLength = None,
    wordSizeBits = 32,
    endian = IrEndian.Big,
    valueType = Some(IrType.Primitive(IrPrimitive.U32)),
    elementSizeBytes = Some(4),
    elementStrideBytes = None,
    decodeCodec = Some(bigU32)
  )

  private val plan = RoutePlan(
    path = "/api/Demo/node",
    reads = List(
      ReadPlan(
        path = "/api/Demo/node",
        address = 0x80001000L,
        sizeBytes = 8,
        decodeType = None,
        endian = IrEndian.Big,
        wordSizeBits = Some(32),
        decodeCodec = Some(bigU32),
        cStringPointer = false
      )
    ),
    memberSubRoutes = List(valueSub, pointerSub)
  )

  private val routes = Map(plan.path -> plan)

  test("HTTP resolves exact root plans") {
    assert(
      RoutePathResolver
        .resolveForHttp("/api/Demo/node", routes)
        .contains(
          ResolvedDataPath.Root(plan)
        )
    )
  }

  test("HTTP prefers shallow MemberSub for pointer members (follow path)") {
    RoutePathResolver.resolveForHttp("/api/Demo/node/next", routes) match {
      case Some(ResolvedDataPath.MemberSub(_, sub: MemberSubRoute.PointerSubRoute, None)) =>
        assert(sub.memberName == "next")
      case other =>
        fail(s"expected MemberSub(PointerSubRoute), got $other")
    }
  }

  test("HTTP resolves value members as MemberSub") {
    RoutePathResolver.resolveForHttp("/api/Demo/node/count", routes) match {
      case Some(ResolvedDataPath.MemberSub(_, sub: MemberSubRoute.ValueSubRoute, None)) =>
        assert(sub.memberName == "count")
      case other =>
        fail(s"expected MemberSub(ValueSubRoute), got $other")
    }
  }

  test("watch resolves exact root plans") {
    assert(
      RoutePathResolver.resolveForWatch("/api/Demo/node", routes) == Right(
        ResolvedDataPath.Root(plan)
      )
    )
  }

  test("watch resolves pointer members as NestedMember pointer slots (no follow)") {
    RoutePathResolver.resolveForWatch("/api/Demo/node/next", routes) match {
      case Right(ResolvedDataPath.NestedMember(resolved)) =>
        assert(resolved.isPointerSlot)
        assert(resolved.address == 0x80001000L + 4)
        assert(resolved.sizeBytes == 4)
      case other =>
        fail(s"expected NestedMember pointer slot, got $other")
    }
  }

  test("watch rejects unknown paths") {
    assert(
      RoutePathResolver
        .resolveForWatch("/api/Demo/missing", routes)
        .swap
        .exists(_.contains("No route generated"))
    )
  }

  test("pointer-chain indexed paths resolve for HTTP and reject for watches") {
    val chainPlan = plan.copy(
      path = "/api/Demo/table",
      pointerChain = Some(
        PointerChainPlan(
          pointeeType = IrType.Primitive(IrPrimitive.U32),
          pointerDepth = 1,
          outerArrayLength = Some(4),
          baseAddress = 0x80002000L,
          endian = IrEndian.Big,
          wordSizeBits = 32,
          pointeeSizeBytes = 4,
          pointeeDecodeCodec = Some(bigU32)
        )
      ),
      memberSubRoutes = Nil
    )
    val chainRoutes = Map(chainPlan.path -> chainPlan)
    RoutePathResolver.resolveForHttp("/api/Demo/table/2", chainRoutes) match {
      case Some(ResolvedDataPath.PointerChain(_, segments)) =>
        assert(segments == List(2))
      case other =>
        fail(s"expected PointerChain, got $other")
    }
    assert(
      RoutePathResolver
        .resolveForWatch("/api/Demo/table/2", chainRoutes)
        .swap
        .exists(_.contains("cannot be watched directly"))
    )
  }
}
