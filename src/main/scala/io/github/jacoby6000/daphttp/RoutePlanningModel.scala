package io.github.jacoby6000.daphttp

import io.circe.Json
import scodec.Codec

final case class ReadPlan(
    path: String,
    address: Long,
    sizeBytes: Int,
    decodeType: Option[IrType],
    wordSizeBits: Option[Int],
    decodeCodec: Option[Codec[Json]]
)
final case class RoutePlan(path: String, reads: List[ReadPlan])
