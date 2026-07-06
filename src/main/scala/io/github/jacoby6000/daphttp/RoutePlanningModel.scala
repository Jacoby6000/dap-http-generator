package io.github.jacoby6000.daphttp

final case class ReadPlan(
    path: String,
    address: Long,
    sizeBytes: Int,
    decodeType: Option[IrType],
    wordSizeBits: Option[Int]
)
final case class RoutePlan(path: String, reads: List[ReadPlan])
