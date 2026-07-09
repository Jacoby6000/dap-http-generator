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
    cStringPointer: Boolean
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
final case class RoutePlan(
    path: String,
    reads: List[ReadPlan],
    pointerChain: Option[PointerChainPlan] = None
)
final case class RoutePlansLoadResult(
    routes: Map[String, RoutePlan],
    errors: List[String]
)
final case class IrGenerationResult(
    warnings: List[String],
    services: List[IrService]
)
