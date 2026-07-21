package io.github.jacoby6000.daphttp

import cats.effect.IO
import cats.effect.Ref
import fs2.concurrent.Topic
import io.circe.Json
import scodec.Codec
import scodec.bits.BitVector

import java.util.Base64
import scala.util.Try

final case class OverlayFieldUpdate(segments: List[String], decoded: Json)

final case class WatchUpdate(
    watchId: Int,
    path: String,
    decoded: Json,
    overlayDecoded: Option[Json],
    overlayFieldUpdates: List[OverlayFieldUpdate] = Nil
)

final case class OverlayWatchField(
    offsetInWatch: Int,
    size: Int,
    segments: List[String],
    codec: Codec[Json],
    irType: Option[IrType]
)

final case class WatchBinding(
    watchId: Int,
    path: String,
    address: Long,
    count: Int,
    sourceOffsetInWatch: Int,
    sourceCount: Int,
    decodeCodec: Codec[Json],
    decodeType: Option[IrType],
    endian: IrEndian,
    wordSizeBits: Option[Int],
    overlayFields: List[OverlayWatchField],
    /** Parent route path (same as `path` for root watches). */
    parentPath: String
)

/** Resolves HTTP data paths to DAP watch regions, decodes `dolphin_memoryChanged` payloads, and
  * broadcasts JSON updates for WebSocket clients.
  */
private[daphttp] final class RealtimeWatchService(
    dapClient: DapHttpServerMain.DapClient,
    plansRef: Ref[IO, RoutePlansLoadResult],
    overlaysRef: Ref[IO, OverlayEngine],
    bindingsRef: Ref[IO, Map[Int, WatchBinding]],
    updates: Topic[IO, WatchUpdate],
    cleared: Topic[IO, Unit]
) {
  def subscribe(path: String): IO[Either[String, WatchBinding]] =
    for {
      plans <- plansRef.get
      engine <- overlaysRef.get
      result <- WatchPathResolver.resolve(path, plans.routes, engine) match {
        case Left(error) =>
          IO.pure(Left(error))
        case Right(target) =>
          dapClient.realtimeWatch(target.address, target.count).flatMap {
            case Left(error) =>
              IO.pure(Left(error))
            case Right(handle) =>
              val binding = WatchBinding(
                watchId = handle.watchId,
                path = path,
                address = handle.address,
                count = handle.count,
                sourceOffsetInWatch = target.sourceOffsetInWatch,
                sourceCount = target.sourceCount,
                decodeCodec = target.decodeCodec,
                decodeType = target.decodeType,
                endian = target.endian,
                wordSizeBits = target.wordSizeBits,
                overlayFields = target.overlayFields,
                parentPath = target.parentPath
              )
              bindingsRef.update(_ + (handle.watchId -> binding)).as(Right(binding))
          }
      }
    } yield result

  def cancel(watchId: Int): IO[Either[String, Unit]] =
    dapClient.realtimeWatchCancel(watchId).flatMap {
      case Left(error) =>
        IO.pure(Left(error))
      case Right(_) =>
        bindingsRef.update(_ - watchId).as(Right(()))
    }

  def list: IO[List[WatchBinding]] =
    bindingsRef.get.map(_.values.toList.sortBy(_.watchId))

  def updatesStream: fs2.Stream[IO, WatchUpdate] =
    updates.subscribe(256)

  def clearedStream: fs2.Stream[IO, Unit] =
    cleared.subscribe(32)

  def start(): IO[Unit] =
    for {
      _ <- dapClient.memoryChanged.evalMap(handleMemoryChanged).compile.drain.start
      _ <- dapClient.sessionResets.evalMap(_ => clearAllLocal).compile.drain.start
    } yield ()

  private def handleMemoryChanged(event: MemoryChangedEvent): IO[Unit] =
    bindingsRef.get.flatMap { bindings =>
      bindings.get(event.watchId) match {
        case None =>
          IO.unit
        case Some(binding) =>
          decodeEvent(binding, event).flatMap {
            case None =>
              IO.unit
            case Some(update) =>
              updates.publish1(update).void
          }
      }
    }

  private def decodeEvent(
      binding: WatchBinding,
      event: MemoryChangedEvent
  ): IO[Option[WatchUpdate]] =
    IO.delay {
      Try(Base64.getDecoder.decode(event.dataBase64)).toOption.flatMap { bytes =>
        val clipped =
          if (bytes.length <= binding.count) bytes
          else java.util.Arrays.copyOf(bytes, binding.count)
        val sourceStart = binding.sourceOffsetInWatch
        val sourceEnd = math.min(clipped.length, sourceStart + binding.sourceCount)
        if (sourceStart < 0 || sourceStart >= clipped.length || sourceEnd <= sourceStart) None
        else {
          val sourceBytes =
            java.util.Arrays.copyOfRange(clipped, sourceStart, sourceEnd)
          binding.decodeCodec
            .decode(BitVector(sourceBytes))
            .toOption
            .map(_.value)
            .map { decoded =>
              val annotated = binding.decodeType
                .map(t =>
                  HttpRouteIrEmitter.annotateDecodedAddresses(
                    t,
                    decoded,
                    binding.address + sourceStart.toLong,
                    binding.wordSizeBits
                  )
                )
                .getOrElse(decoded)
              (clipped, annotated)
            }
        }
      }
    }.flatMap {
      case None =>
        IO.pure(None)
      case Some((watchBytes, decoded)) =>
        overlaysRef.get.flatMap { engine =>
          val fieldUpdates = decodeOverlayFields(binding, watchBytes)
          decodeFullOverlay(binding, watchBytes, engine).map { fullOverlay =>
            Some(
              WatchUpdate(
                watchId = binding.watchId,
                path = binding.path,
                decoded = decoded,
                overlayDecoded = fullOverlay,
                overlayFieldUpdates = fieldUpdates
              )
            )
          }
        }
    }

  private def decodeOverlayFields(
      binding: WatchBinding,
      watchBytes: Array[Byte]
  ): List[OverlayFieldUpdate] =
    binding.overlayFields.flatMap { field =>
      val start = field.offsetInWatch
      val end = math.min(watchBytes.length, start + field.size)
      if (start < 0 || start >= watchBytes.length || end <= start) None
      else {
        val slice = java.util.Arrays.copyOfRange(watchBytes, start, end)
        field.codec.decode(BitVector(slice)).toOption.map { result =>
          val annotated = field.irType
            .map(t =>
              HttpRouteIrEmitter.annotateDecodedAddresses(
                t,
                result.value,
                binding.address + start.toLong,
                binding.wordSizeBits
              )
            )
            .getOrElse(result.value)
          OverlayFieldUpdate(field.segments, annotated)
        }
      }
    }

  private def decodeFullOverlay(
      binding: WatchBinding,
      watchBytes: Array[Byte],
      engine: OverlayEngine
  ): IO[Option[Json]] =
    // Full-struct overlay decode only when the watch covers the root read (no field mapping).
    if (binding.overlayFields.nonEmpty || binding.path != binding.parentPath)
      IO.pure(None)
    else
      binding.decodeType match {
        case None =>
          IO.pure(None)
        case Some(irType) =>
          val (_, prep) = engine.prepare(irType, binding.endian, binding.wordSizeBits)
          prep match {
            case None =>
              IO.pure(None)
            case Some(prepared) =>
              IO.delay {
                val take = math.min(watchBytes.length, prepared.sizeBytes)
                prepared.codec
                  .decode(BitVector(java.util.Arrays.copyOf(watchBytes, take)))
                  .toOption
                  .map { result =>
                    HttpRouteIrEmitter.annotateDecodedAddresses(
                      prepared.irType,
                      result.value,
                      binding.address,
                      binding.wordSizeBits
                    )
                  }
              }
          }
      }

  private def clearAllLocal: IO[Unit] =
    for {
      _ <- bindingsRef.set(Map.empty)
      _ <- cleared.publish1(())
    } yield ()
}

private[daphttp] object RealtimeWatchService {
  def create(
      dapClient: DapHttpServerMain.DapClient,
      plansRef: Ref[IO, RoutePlansLoadResult],
      overlaysRef: Ref[IO, OverlayEngine]
  ): IO[RealtimeWatchService] =
    for {
      bindings <- Ref.of[IO, Map[Int, WatchBinding]](Map.empty)
      updates <- Topic[IO, WatchUpdate]
      cleared <- Topic[IO, Unit]
    } yield new RealtimeWatchService(
      dapClient,
      plansRef,
      overlaysRef,
      bindings,
      updates,
      cleared
    )
}

private[daphttp] final case class WatchTarget(
    address: Long,
    count: Int,
    sourceOffsetInWatch: Int,
    sourceCount: Int,
    decodeCodec: Codec[Json],
    decodeType: Option[IrType],
    endian: IrEndian,
    wordSizeBits: Option[Int],
    overlayFields: List[OverlayWatchField],
    parentPath: String
)

private[daphttp] object WatchPathResolver {
  def resolve(
      path: String,
      routes: Map[String, RoutePlan],
      overlayEngine: OverlayEngine = OverlayEngine.empty
  ): Either[String, WatchTarget] = {
    DapHttpServerMain.matchRoutePublic(path, routes) match {
      case Some((plan, Nil)) =>
        plan.reads.headOption match {
          case Some(read) =>
            read.decodeCodec match {
              case Some(codec) =>
                val base = WatchTarget(
                  address = read.address,
                  count = read.sizeBytes,
                  sourceOffsetInWatch = 0,
                  sourceCount = read.sizeBytes,
                  decodeCodec = codec,
                  decodeType = read.decodeType,
                  endian = read.endian,
                  wordSizeBits = read.wordSizeBits,
                  overlayFields = Nil,
                  parentPath = path
                )
                Right(expandRootWatchForOverlay(base, read.decodeType, overlayEngine))
              case None =>
                Left(s"Route $path has no decode codec for watching.")
            }
          case None =>
            Left(s"Route $path has no readable memory plan.")
        }
      case Some((_, _ :: _)) =>
        Left(s"Pointer-chain path $path cannot be watched directly; open a concrete index first.")
      case None =>
        MemberPathResolver.resolve(path, routes) match {
          case None =>
            Left(s"No route generated for $path")
          case Some(resolved) =>
            resolveResolvedMember(
              path,
              resolved,
              routes.get(resolved.parentPath),
              overlayEngine
            )
        }
    }
  }

  private def resolveResolvedMember(
      path: String,
      resolved: ResolvedMemberRead,
      parentPlan: Option[RoutePlan],
      overlayEngine: OverlayEngine
  ): Either[String, WatchTarget] =
    if (resolved.sizeBytes <= 0)
      Left(s"Unable to determine watch size for $path")
    else
      resolved.decodeCodec match {
        case None =>
          Left(s"Member $path has no decode codec for watching.")
        case Some(codec) =>
          val base = WatchTarget(
            address = resolved.address,
            count = resolved.sizeBytes,
            sourceOffsetInWatch = 0,
            sourceCount = resolved.sizeBytes,
            decodeCodec = codec,
            decodeType = resolved.valueType,
            endian = resolved.endian,
            wordSizeBits = Some(resolved.wordSizeBits),
            overlayFields = Nil,
            parentPath = resolved.parentPath
          )
          if (resolved.isPointerSlot)
            Right(base)
          else {
            val withParentOverlap = parentPlan match {
              case Some(plan) =>
                expandWithOverlayMapping(
                  base,
                  plan,
                  resolved.sourceOffsetInParent,
                  overlayEngine
                )
              case None =>
                base
            }
            Right(expandRootWatchForOverlay(withParentOverlap, resolved.valueType, overlayEngine))
          }
      }

  /** Expand a root watch's DAP byte count when the overlay layout is wider than the source. Keep
    * `overlayFields` empty so [[RealtimeWatchService.decodeFullOverlay]] still runs.
    */
  private def expandRootWatchForOverlay(
      base: WatchTarget,
      decodeType: Option[IrType],
      overlayEngine: OverlayEngine
  ): WatchTarget =
    decodeType match {
      case Some(irType)
          if TypeOverlay.affectsDecode(
            irType,
            overlayEngine.document,
            overlayEngine.typeIndex
          ) =>
        TypeOverlay
          .compileOverlayCodec(
            irType,
            overlayEngine.document,
            overlayEngine.typeIndex,
            base.endian,
            base.wordSizeBits
          )
          .toOption
          .map { case (_, _, overlaySize) =>
            if (overlaySize > base.count)
              base.copy(count = overlaySize)
            else base
          }
          .getOrElse(base)
      case _ =>
        base
    }

  /** Expand the DAP watch region to cover overlay members that overlap the source member, and
    * attach per-field overlay decode plans for WS updates.
    */
  private def expandWithOverlayMapping(
      base: WatchTarget,
      plan: RoutePlan,
      sourceOffsetInStruct: Int,
      overlayEngine: OverlayEngine
  ): WatchTarget = {
    val wordSize = base.wordSizeBits
    val parentType = plan.reads.headOption.flatMap(_.decodeType)
    val structBase =
      plan.reads.headOption.map(_.address).getOrElse(base.address - sourceOffsetInStruct)
    parentType match {
      case Some(sourceStruct: IrType.Struct)
          if TypeOverlay.affectsDecode(
            sourceStruct,
            overlayEngine.document,
            overlayEngine.typeIndex
          ) =>
        TypeOverlay.rewriteType(
          sourceStruct,
          overlayEngine.document,
          overlayEngine.typeIndex,
          wordSize
        ) match {
          case Left(_) =>
            base
          case Right(overlayType) =>
            overlayType match {
              case overlayStruct: IrType.Struct =>
                OverlayFieldMapping.overlappingOverlaySpansInRange(
                  overlayStruct,
                  sourceOffsetInStruct,
                  base.sourceCount,
                  wordSize
                ) match {
                  case Left(_) | Right(Nil) =>
                    base
                  case Right(overlaySpans) =>
                    val ranges =
                      (sourceOffsetInStruct, base.sourceCount) ::
                        overlaySpans.map(s => (s.offset, s.size))
                    OverlayFieldMapping.unionRange(ranges) match {
                      case None =>
                        base
                      case Some((unionStart, unionSize)) =>
                        val fields = overlaySpans.flatMap { span =>
                          HttpRouteIrEmitter
                            .compileCodec(span.irType, base.endian, wordSize)
                            .toOption
                            .map { codec =>
                              OverlayWatchField(
                                offsetInWatch = span.offset - unionStart,
                                size = span.size,
                                segments = List(span.name),
                                codec = codec,
                                irType = Some(span.irType)
                              )
                            }
                        }
                        base.copy(
                          address = structBase + unionStart.toLong,
                          count = unionSize,
                          sourceOffsetInWatch = sourceOffsetInStruct - unionStart,
                          overlayFields = fields
                        )
                    }
                }
              case _ =>
                base
            }
        }
      case _ =>
        base
    }
  }
}
