package io.github.jacoby6000.daphttp

import cats.data.OptionT
import cats.effect.IO
import cats.effect.Ref
import fs2.Pipe
import io.circe.Json
import io.circe.syntax._
import org.http4s.HttpRoutes
import org.http4s.MediaType
import org.http4s.Request
import org.http4s.Response
import org.http4s.StaticFile
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.io._
import org.http4s.headers.`Content-Type`
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import software.amazon.smithy.model.shapes.ShapeId

import java.nio.file.Path
import java.nio.file.Paths
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._

/** Browser explorer + catalog endpoints (not generated data, not raw DAP). */
private[daphttp] object WebAppRoutes {

  def routes(
      plansRef: Ref[IO, RoutePlansLoadResult],
      overlaysRef: Ref[IO, OverlayEngine],
      overlayPersistPath: Option[Path],
      watchService: RealtimeWatchService,
      wsBuilder: WebSocketBuilder2[IO]
  ): HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case GET -> Root / "health" =>
        Ok(Json.obj("status" -> Json.fromString("ok")))

      case GET -> Root / "routes" =>
        plansRef.get.flatMap { result =>
          Ok(
            Json.obj(
              "routes" -> RouteTree.flatPaths(result.routes).asJson,
              "tree" -> RouteTree.fromPlans(result.routes).asJson,
              "errors" -> result.errors.asJson
            )
          )
        }

      case GET -> Root / "watches" if watchService != null =>
        watchService.list.flatMap { watches =>
          Ok(
            Json.obj(
              "watches" -> watches
                .map(w =>
                  Json.obj(
                    "watchId" -> Json.fromInt(w.watchId),
                    "path" -> Json.fromString(w.path),
                    "address" -> Json.fromString(f"0x${w.address}%x"),
                    "count" -> Json.fromInt(w.count),
                    "overlaySegments" -> w.overlayFields.map(f => f.segments.asJson).asJson
                  )
                )
                .asJson
            )
          )
        }

      case req @ POST -> Root / "watches" if watchService != null =>
        req.as[Json].flatMap { body =>
          body.hcursor.get[String]("path") match {
            case Left(_) =>
              BadRequest(Json.obj("error" -> Json.fromString("Missing path")))
            case Right(path) =>
              watchService.subscribe(path).flatMap {
                case Left(error) =>
                  BadRequest(Json.obj("error" -> Json.fromString(error)))
                case Right(binding) =>
                  Ok(
                    Json.obj(
                      "watchId" -> Json.fromInt(binding.watchId),
                      "path" -> Json.fromString(binding.path),
                      "address" -> Json.fromString(f"0x${binding.address}%x"),
                      "count" -> Json.fromInt(binding.count),
                      "overlaySegments" -> binding.overlayFields
                        .map(f => f.segments.asJson)
                        .asJson
                    )
                  )
              }
          }
        }

      case DELETE -> Root / "watches" / IntVar(watchId) if watchService != null =>
        watchService.cancel(watchId).flatMap {
          case Left(error) =>
            NotFound(Json.obj("error" -> Json.fromString(error)))
          case Right(_) =>
            Ok(Json.obj("status" -> Json.fromString("ok")))
        }

      case GET -> Root / "ws" if watchService != null && wsBuilder != null =>
        // DESNOTE(jbarber, 2026-07-20): Ember's default idle timeout is 60s; quiet watch
        // sockets with no memoryChanged frames get killed. Periodic Ping frames keep the
        // connection active (browsers auto-reply with Pong).
        val toClient = watchService.updatesStream
          .map { update =>
            val payload = Json.obj(
              "type" -> Json.fromString("memoryChanged"),
              "watchId" -> Json.fromInt(update.watchId),
              "path" -> Json.fromString(update.path),
              "decoded" -> update.decoded,
              "overlayDecoded" -> update.overlayDecoded.getOrElse(Json.Null),
              "overlayUpdates" -> update.overlayFieldUpdates
                .map(u =>
                  Json.obj(
                    "segments" -> u.segments.asJson,
                    "decoded" -> u.decoded
                  )
                )
                .asJson
            )
            WebSocketFrame.Text(payload.noSpaces)
          }
          .merge(
            watchService.clearedStream.map { _ =>
              WebSocketFrame.Text(
                Json.obj("type" -> Json.fromString("watchesCleared")).noSpaces
              )
            }
          )
          .merge(
            watchService.reboundStream.map { watches =>
              WebSocketFrame.Text(
                Json
                  .obj(
                    "type" -> Json.fromString("watchesRebound"),
                    "watches" -> watches
                      .map(w =>
                        Json.obj(
                          "watchId" -> Json.fromInt(w.watchId),
                          "path" -> Json.fromString(w.path),
                          "address" -> Json.fromString(f"0x${w.address}%x"),
                          "count" -> Json.fromInt(w.count),
                          "overlaySegments" -> w.overlayFields
                            .map(f => f.segments.asJson)
                            .asJson
                        )
                      )
                      .asJson
                  )
                  .noSpaces
              )
            }
          )
          .merge(
            fs2.Stream
              .awakeEvery[IO](20.seconds)
              .as(WebSocketFrame.Ping())
          )
        val fromClient: Pipe[IO, WebSocketFrame, Unit] = _.drain
        wsBuilder.build(toClient, fromClient)

      case GET -> Root / "types" =>
        for {
          plans <- plansRef.get
          engine <- overlaysRef.get
          // DESNOTE(jbarber, 2026-07-20): Omit per-struct `fields` from the catalog payload.
          // Melee-scale IR makes full field lists enormous; the editor fetches fields for one
          // struct via /types/fields when needed.
          response <- Ok(
            Json.obj("types" -> TypeOverlay.catalog(plans.services, engine.document).asJson)
          )
        } yield response

      case request @ GET -> Root / "types" / "fields" =>
        request.uri.query.params.get("id") match {
          case None | Some("") =>
            BadRequest(Json.obj("error" -> Json.fromString("Query parameter id is required")))
          case Some(typeId) =>
            for {
              plans <- plansRef.get
              engine <- overlaysRef.get
              response <- TypeOverlay.fieldsFor(plans.services, engine.document, typeId) match {
                case None =>
                  NotFound(Json.obj("error" -> Json.fromString(s"Unknown type id: $typeId")))
                case Some(fields) =>
                  Ok(
                    Json.obj(
                      "id" -> Json.fromString(typeId),
                      "fields" -> Json.fromValues(
                        fields.map(TypeOverlayDocument.overlayMemberEncoder.apply)
                      )
                    )
                  )
              }
            } yield response
        }

      case GET -> Root / "overlays" =>
        overlaysRef.get.flatMap(engine => Ok(engine.document.asJson))

      case request @ PUT -> Root / "overlays" =>
        request.as[Json].flatMap { json =>
          json.as[TypeOverlayDocument] match {
            case Left(err) =>
              BadRequest(Json.obj("error" -> Json.fromString(err.getMessage)))
            case Right(raw) =>
              TypeOverlay.validate(raw) match {
                case Left(errors) =>
                  BadRequest(Json.obj("errors" -> errors.asJson))
                case Right(document) =>
                  for {
                    plans <- plansRef.get
                    typeIndex = TypeOverlay.buildTypeIndex(plans.services)
                    normalized = normalizeOverlayKeys(document, typeIndex)
                    validationErrors = validateOverlayTypes(normalized, typeIndex)
                    response <-
                      if (validationErrors.nonEmpty)
                        BadRequest(Json.obj("errors" -> validationErrors.asJson))
                      else {
                        val engine = OverlayEngine.fromServices(normalized, plans.services)
                        for {
                          _ <- overlaysRef.set(engine)
                          _ <- IO.blocking {
                            overlayPersistPath.foreach(TypeOverlayDocument.save(_, normalized))
                          }
                          rebindResult <-
                            if (watchService != null) watchService.rebindAll
                            else IO.pure((List.empty[WatchBinding], List.empty[String]))
                          (watches, watchErrors) = rebindResult
                          response <- Ok(
                            normalized.asJson.mapObject { obj =>
                              val withWatches = obj.add(
                                "watches",
                                watches
                                  .map(w =>
                                    Json.obj(
                                      "watchId" -> Json.fromInt(w.watchId),
                                      "path" -> Json.fromString(w.path),
                                      "address" -> Json.fromString(f"0x${w.address}%x"),
                                      "count" -> Json.fromInt(w.count),
                                      "overlaySegments" -> w.overlayFields
                                        .map(f => f.segments.asJson)
                                        .asJson
                                    )
                                  )
                                  .asJson
                              )
                              if (watchErrors.isEmpty) withWatches
                              else withWatches.add("watchErrors", watchErrors.asJson)
                            }
                          )
                        } yield response
                      }
                  } yield response
              }
          }
        }

      case request @ GET -> Root =>
        serveWebAsset(request, "index.html")

      case request @ GET -> Root / "assets" / fileName =>
        serveWebAsset(request, fileName)
    }

  /** Prefer canonical `namespace#name` keys so overlay lookup matches IR shape ids. */
  private def normalizeOverlayKeys(
      document: TypeOverlayDocument,
      typeIndex: Map[ShapeId, IrType]
  ): TypeOverlayDocument = {
    val normalizedStructs = document.structs.map { case (key, defn) =>
      val canonical =
        try {
          val shapeId =
            if (key.contains("#")) ShapeId.from(key)
            else
              typeIndex
                .collectFirst { case (id, _) if id.getName == key => id }
                .getOrElse(TypeOverlay.normalizeShapeId(key))
          shapeId.toString
        } catch {
          case _: IllegalArgumentException =>
            TypeOverlay.normalizeShapeId(key).toString
        }
      canonical -> defn
    }
    document.copy(structs = normalizedStructs)
  }

  private def validateOverlayTypes(
      document: TypeOverlayDocument,
      typeIndex: Map[ShapeId, IrType]
  ): List[String] = {
    val errors = ListBuffer.empty[String]
    def checkMembers(context: String, members: List[OverlayMember]): Unit =
      members.foreach { member =>
        TypeOverlay.resolveTypeId(member.typeId, document, typeIndex) match {
          case Left(err) => errors += s"$context.${member.name}: $err"
          case Right(_)  => ()
        }
      }
    document.structs.foreach { case (id, defn) =>
      checkMembers(s"structs[$id]", defn.members)
    }
    document.newStructs.foreach { ns =>
      checkMembers(s"newStructs[${ns.id}]", ns.members)
    }
    errors.toList.distinct
  }

  private def serveWebAsset(request: Request[IO], fileName: String): IO[Response[IO]] = {
    val safeName = Paths.get(fileName).getFileName.toString
    val resourcePath = s"/web/$safeName"
    StaticFile
      .fromResource[IO](resourcePath, Some(request))
      .orElse {
        // Fallback when the Scala.js bundle has not been packaged yet (e.g. tests).
        if (safeName == "index.html")
          OptionT.liftF(Ok(fallbackIndexHtml, `Content-Type`(MediaType.text.html)))
        else OptionT.none[IO, Response[IO]]
      }
      .getOrElseF(NotFound())
  }

  private val fallbackIndexHtml: String =
    """<!DOCTYPE html>
      |<html lang="en">
      |<head>
      |  <meta charset="utf-8"/>
      |  <title>dap-http</title>
      |  <style>
      |    body { font-family: ui-monospace, monospace; margin: 2rem; background: #0f1419; color: #e7ecf1; }
      |    a { color: #7eb8ff; }
      |  </style>
      |</head>
      |<body>
      |  <h1>dap-http</h1>
      |  <p>UI assets are not packaged. Run <code>sbt compile</code> to build the Scala.js bundle, or use the JSON API:</p>
      |  <ul>
      |    <li><a href="/routes">/routes</a></li>
      |    <li><a href="/health">/health</a></li>
      |  </ul>
      |</body>
      |</html>
      |""".stripMargin
}
