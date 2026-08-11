package io.github.jacoby6000.daphttp

import io.circe.Json
import scodec.Codec
import software.amazon.smithy.model.shapes.ShapeId

/** In-memory overlay document plus caches used on the decode hot path. */
final case class OverlayEngine(
    document: TypeOverlayDocument,
    typeIndex: Map[ShapeId, IrType],
    codecCache: Map[OverlayEngine.CacheKey, OverlayEngine.PreparedCodec] = Map.empty
) {
  def withDocument(
      next: TypeOverlayDocument,
      services: List[IrService]
  ): OverlayEngine =
    OverlayEngine(
      document = next,
      typeIndex = TypeOverlay.buildTypeIndex(services),
      codecCache = Map.empty
    )

  def refreshIndex(services: List[IrService]): OverlayEngine =
    copy(
      typeIndex = TypeOverlay.buildTypeIndex(services),
      codecCache = Map.empty
    )

  def prepare(
      decodeType: IrType,
      endian: IrEndian,
      wordSize: Option[Int]
  ): (OverlayEngine, Option[OverlayEngine.PreparedCodec]) = {
    if (!TypeOverlay.affectsDecode(decodeType, document, typeIndex)) {
      (this, None)
    } else {
      val key = OverlayEngine.CacheKey(
        TypeOverlay.rootTypeKey(decodeType),
        endian,
        wordSize
      )
      codecCache.get(key) match {
        case Some(prepared) =>
          (this, Some(prepared))
        case None =>
          TypeOverlay.compileOverlayCodec(
            decodeType,
            document,
            typeIndex,
            endian,
            wordSize
          ) match {
            case Left(_) =>
              (this, None)
            case Right((rewritten, codec, sizeBytes)) =>
              val prepared = OverlayEngine.PreparedCodec(rewritten, codec, sizeBytes)
              (copy(codecCache = codecCache + (key -> prepared)), Some(prepared))
          }
      }
    }
  }
}

object OverlayEngine {
  final case class CacheKey(typeKey: String, endian: IrEndian, wordSize: Option[Int])
  final case class PreparedCodec(irType: IrType, codec: Codec[Json], sizeBytes: Int)

  def empty: OverlayEngine =
    OverlayEngine(TypeOverlayDocument.empty, Map.empty)

  def fromServices(
      document: TypeOverlayDocument,
      services: List[IrService]
  ): OverlayEngine =
    OverlayEngine(
      document = document,
      typeIndex = TypeOverlay.buildTypeIndex(services),
      codecCache = Map.empty
    )
}
