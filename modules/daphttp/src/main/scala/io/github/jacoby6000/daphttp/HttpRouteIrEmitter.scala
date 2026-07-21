package io.github.jacoby6000.daphttp

import io.circe.Json
import scodec.Codec

/** Compatibility façade for route planning and IR JSON codecs.
  *
  * DESNOTE(jbarber, 2026-07-21): Implementation lives in [[RoutePlanEmitter]] and [[IrJsonCodecs]];
  * this object keeps the historical call sites stable during migration.
  */
object HttpRouteIrEmitter {
  def emitRoutePlansFromIr(irServices: List[IrService]): RoutePlansLoadResult =
    RoutePlanEmitter.emitRoutePlansFromIr(irServices)

  def compileCodec(
      irType: IrType,
      endian: IrEndian,
      wordSize: Option[Int]
  ): Either[List[String], Codec[Json]] =
    IrJsonCodecs.compileCodec(irType, endian, wordSize)

  def sizeBytesForType(
      irType: IrType,
      wordSize: Option[Int]
  ): Either[List[String], Int] =
    IrJsonCodecs.sizeBytesForType(irType, wordSize)

  def codecForType(
      irType: IrType,
      endian: IrEndian,
      wordSize: Option[Int]
  ): Option[Codec[Json]] =
    IrJsonCodecs.codecForType(irType, endian, wordSize)

  def annotateDecodedAddresses(
      irType: IrType,
      decoded: Json,
      baseAddress: Long,
      wordSize: Option[Int],
      elementStrideBytes: Option[Int] = None
  ): Json =
    IrJsonCodecs.annotateDecodedAddresses(
      irType,
      decoded,
      baseAddress,
      wordSize,
      elementStrideBytes
    )
}
