package io.github.jacoby6000.daphttp

import cats.effect.IO
import cats.effect.Ref
import io.circe.Json
import scodec.Codec
import scodec.bits.BitVector

import java.util.Base64
import scala.util.Try

/** Shared DAP memory read + source/overlay decode for a single address region.
  *
  * HTTP handlers keep envelope/JSON shape differences; this object owns the repeated prepare → read
  * → truncate → decode → annotate → overlay sequence.
  */
private[daphttp] object MemoryDecodeService {

  final case class Region(
      address: Long,
      sourceSizeBytes: Int,
      endian: IrEndian,
      wordSizeBits: Int,
      valueType: Option[IrType],
      decodeCodec: Option[Codec[Json]]
  )

  sealed trait Outcome
  object Outcome {
    final case class Success(
        readSize: Int,
        rawBase64: String,
        decoded: Json,
        overlayDecoded: Option[Json]
    ) extends Outcome
    final case class Failed(error: String) extends Outcome
  }

  def prepareOverlay(
      overlaysRef: Ref[IO, OverlayEngine],
      decodeType: Option[IrType],
      endian: IrEndian,
      wordSizeBits: Option[Int]
  ): IO[Option[OverlayEngine.PreparedCodec]] =
    decodeType match {
      case None =>
        IO.pure(None)
      case Some(irType) =>
        overlaysRef.modify { engine =>
          engine.prepare(irType, endian, wordSizeBits)
        }
    }

  def truncateBase64ToBytes(base64Data: String, sizeBytes: Int): String =
    Try(Base64.getDecoder.decode(base64Data)).toOption
      .map { bytes =>
        val truncated =
          if (bytes.length <= sizeBytes) bytes
          else java.util.Arrays.copyOf(bytes, sizeBytes)
        Base64.getEncoder.encodeToString(truncated)
      }
      .getOrElse(base64Data)

  def decodeWithCodec(base64Data: String, codec: Option[Codec[Json]]): Json =
    codec match {
      case None =>
        Json.Null
      case Some(c) =>
        Try(Base64.getDecoder.decode(base64Data)).toOption
          .flatMap(bytes => c.decode(BitVector(bytes)).toOption.map(_.value))
          .getOrElse(Json.Null)
    }

  /** Read one memory region and decode source (+ optional overlay) JSON. */
  def readAndDecode(
      region: Region,
      dapClient: DapClient,
      overlaysRef: Ref[IO, OverlayEngine],
      resolveAndAnnotate: (
          Option[IrType],
          Json,
          Long,
          Int,
          IrEndian,
          DapClient
      ) => IO[Json],
      decodeOverlay: (
          OverlayEngine.PreparedCodec,
          String,
          Long,
          Int,
          IrEndian,
          DapClient
      ) => IO[Json]
  ): IO[Outcome] =
    if (region.sourceSizeBytes <= 0)
      IO.pure(Outcome.Failed("Unable to determine member size."))
    else
      prepareOverlay(
        overlaysRef,
        region.valueType,
        region.endian,
        Some(region.wordSizeBits)
      ).flatMap { overlayPrep =>
        val readSize =
          overlayPrep
            .map(o => math.max(region.sourceSizeBytes, o.sizeBytes))
            .getOrElse(region.sourceSizeBytes)
        dapClient.readMemory(region.address, readSize).flatMap {
          case Left(error) =>
            IO.pure(Outcome.Failed(error))
          case Right(data) =>
            val sourceData = truncateBase64ToBytes(data, region.sourceSizeBytes)
            val decoded = decodeWithCodec(sourceData, region.decodeCodec)
            for {
              finalDecoded <- resolveAndAnnotate(
                region.valueType,
                decoded,
                region.address,
                region.wordSizeBits,
                region.endian,
                dapClient
              )
              overlayDecoded <- overlayPrep match {
                case Some(prep) =>
                  decodeOverlay(
                    prep,
                    data,
                    region.address,
                    region.wordSizeBits,
                    region.endian,
                    dapClient
                  ).map(Some(_))
                case None =>
                  IO.pure(None)
              }
            } yield Outcome.Success(readSize, data, finalDecoded, overlayDecoded)
        }
      }
}
