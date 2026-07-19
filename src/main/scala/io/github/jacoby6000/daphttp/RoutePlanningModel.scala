package io.github.jacoby6000.daphttp

import io.circe.Json
import scodec.Codec

final case class ReadPlan(
    path: String,
    address: Long,
    sizeBytes: Int,
    decodeType: Option[IrType],
    endian: IrEndian,
    wordSizeBits: Option[Int],
    decodeCodec: Option[Codec[Json]],
    cStringPointer: Boolean,
    cStringPointerArray: Boolean = false,
    /** Byte stride between array elements when symbol size exceeds packed layout width. */
    elementStrideBytes: Option[Int] = None
)
final case class PointerChainPlan(
    pointeeType: IrType,
    pointerDepth: Int,
    outerArrayLength: Option[Int],
    baseAddress: Long,
    endian: IrEndian,
    wordSizeBits: Int,
    pointeeSizeBytes: Int,
    pointeeDecodeCodec: Option[Codec[Json]],
    followCString: Boolean = false
)
sealed trait MemberSubRoute {
  def memberName: String
  def baseAddress: Long
  def memberOffsetBytes: Int
  def isArray: Boolean
  def arrayLength: Option[Int]
  def wordSizeBits: Int
  def endian: IrEndian
}
object MemberSubRoute {
  final case class ValueSubRoute(
      memberName: String,
      baseAddress: Long,
      memberOffsetBytes: Int,
      isArray: Boolean,
      arrayLength: Option[Int],
      wordSizeBits: Int,
      endian: IrEndian,
      valueType: Option[IrType],
      elementSizeBytes: Option[Int],
      decodeCodec: Option[Codec[Json]]
  ) extends MemberSubRoute

  final case class PointerSubRoute(
      memberName: String,
      baseAddress: Long,
      memberOffsetBytes: Int,
      isArray: Boolean,
      arrayLength: Option[Int],
      wordSizeBits: Int,
      endian: IrEndian,
      pointeeType: Option[IrType],
      pointeeSizeBytes: Option[Int],
      pointeeDecodeCodec: Option[Codec[Json]],
      followCString: Boolean
  ) extends MemberSubRoute
}
final case class RoutePlan(
    path: String,
    reads: List[ReadPlan],
    pointerChain: Option[PointerChainPlan] = None,
    memberSubRoutes: List[MemberSubRoute] = Nil
)
final case class RoutePlansLoadResult(
    routes: Map[String, RoutePlan],
    errors: List[String]
)
final case class IrGenerationResult(
    warnings: List[String],
    services: List[IrService]
)
