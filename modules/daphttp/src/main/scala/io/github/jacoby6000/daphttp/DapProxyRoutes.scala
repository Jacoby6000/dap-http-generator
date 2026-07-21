package io.github.jacoby6000.daphttp

import cats.effect.IO
import cats.effect.Ref
import io.circe.Json
import org.http4s.HttpRoutes
import org.http4s.Response
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.io._
import software.amazon.smithy.model.shapes.ShapeId

import java.util.Base64

/** Thin HTTP façade over DAP runtime commands (`continue`, `readMemory`, `writeMemory`).
  *
  * Paths mirror DAP request names under `/dap-proxy`. Request bodies follow DAP argument shapes
  * where practical; `writeMemory` also accepts a typed leaf form used by the explorer (value +
  * decodeType + segments), which is encoded then forwarded as DAP `writeMemory`.
  *
  * See https://microsoft.github.io/debug-adapter-protocol/specification
  */
private[daphttp] object DapProxyRoutes {
  val Prefix: String = "/dap-proxy"

  def routes(
      plansRef: Ref[IO, RoutePlansLoadResult],
      dapClient: DapClient,
      overlaysRef: Ref[IO, OverlayEngine]
  ): HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case req @ POST -> Root / "dap-proxy" / "continue" =>
        req.as[Json].attempt.flatMap { bodyAttempt =>
          val threadId = bodyAttempt.toOption
            .flatMap(_.hcursor.get[Int]("threadId").toOption)
          dapClient.continueExecution(threadId).flatMap {
            case Right(body) =>
              Ok(
                Json.obj(
                  "command" -> Json.fromString("continue"),
                  "success" -> Json.True,
                  "body" -> body
                )
              )
            case Left(error) =>
              InternalServerError(
                Json.obj(
                  "command" -> Json.fromString("continue"),
                  "success" -> Json.False,
                  "message" -> Json.fromString(error)
                )
              )
          }
        }

      case req @ POST -> Root / "dap-proxy" / "readMemory" =>
        req.as[Json].flatMap { body =>
          val cursor = body.hcursor
          val addressOpt = cursor
            .get[String]("memoryReference")
            .toOption
            .orElse(cursor.get[String]("address").toOption)
            .flatMap(parseHexAddress)
          val countOpt = cursor.get[Int]("count").toOption.filter(_ > 0)
          val offset = cursor.get[Int]("offset").getOrElse(0)
          (addressOpt, countOpt) match {
            case (None, _) =>
              BadRequest(
                Json.obj(
                  "command" -> Json.fromString("readMemory"),
                  "success" -> Json.False,
                  "message" -> Json.fromString("Missing or invalid memoryReference")
                )
              )
            case (_, None) =>
              BadRequest(
                Json.obj(
                  "command" -> Json.fromString("readMemory"),
                  "success" -> Json.False,
                  "message" -> Json.fromString("Missing or invalid count")
                )
              )
            case (Some(address), Some(count)) =>
              val readAddress = address + offset.toLong
              dapClient.readMemory(readAddress, count).flatMap {
                case Left(error) =>
                  InternalServerError(
                    Json.obj(
                      "command" -> Json.fromString("readMemory"),
                      "success" -> Json.False,
                      "message" -> Json.fromString(error)
                    )
                  )
                case Right(data) =>
                  Ok(
                    Json.obj(
                      "command" -> Json.fromString("readMemory"),
                      "success" -> Json.True,
                      "body" -> Json.obj(
                        "address" -> Json.fromString(f"0x$readAddress%x"),
                        "data" -> Json.fromString(data)
                      )
                    )
                  )
              }
          }
        }

      case req @ POST -> Root / "dap-proxy" / "writeMemory" =>
        req.as[Json].flatMap { body =>
          val cursor = body.hcursor
          val dataOpt = cursor.get[String]("data").toOption
          val addressOpt = cursor
            .get[String]("memoryReference")
            .toOption
            .orElse(cursor.get[String]("address").toOption)
            .flatMap(parseHexAddress)
          val offset = cursor.get[Int]("offset").getOrElse(0)

          (addressOpt, dataOpt) match {
            case (Some(address), Some(data)) =>
              val writeAddress = address + offset.toLong
              dapClient.writeMemory(writeAddress, data).flatMap {
                case Left(error) =>
                  InternalServerError(
                    Json.obj(
                      "command" -> Json.fromString("writeMemory"),
                      "success" -> Json.False,
                      "message" -> Json.fromString(error)
                    )
                  )
                case Right(bytesWritten) =>
                  Ok(
                    Json.obj(
                      "command" -> Json.fromString("writeMemory"),
                      "success" -> Json.True,
                      "body" -> Json.obj(
                        "bytesWritten" -> Json.fromInt(bytesWritten),
                        "offset" -> Json.fromInt(offset)
                      )
                    )
                  )
              }
            case _ =>
              // Typed leaf write used by the explorer (encode JSON value then DAP writeMemory).
              val typedAddress = cursor
                .get[String]("address")
                .toOption
                .flatMap(parseHexAddress)
              val valueOpt = cursor.downField("value").focus
              val segments = cursor.get[List[String]]("segments").getOrElse(Nil)
              val decodeTypeOpt = cursor.get[String]("decodeType").toOption
              val useOverlay = cursor.get[Boolean]("overlay").getOrElse(false)
              (typedAddress, valueOpt, decodeTypeOpt) match {
                case (None, _, _) =>
                  BadRequest(
                    Json.obj(
                      "command" -> Json.fromString("writeMemory"),
                      "success" -> Json.False,
                      "message" -> Json.fromString(
                        "Expected memoryReference+data, or address+value+decodeType"
                      )
                    )
                  )
                case (_, None, _) =>
                  BadRequest(
                    Json.obj(
                      "command" -> Json.fromString("writeMemory"),
                      "success" -> Json.False,
                      "message" -> Json.fromString("Missing value")
                    )
                  )
                case (_, _, None) =>
                  BadRequest(
                    Json.obj(
                      "command" -> Json.fromString("writeMemory"),
                      "success" -> Json.False,
                      "message" -> Json.fromString("Missing decodeType")
                    )
                  )
                case (Some(address), Some(value), Some(decodeType)) =>
                  for {
                    plans <- plansRef.get
                    engine <- overlaysRef.get
                    response <- writeTypedField(
                      plans.services,
                      engine,
                      decodeType,
                      segments,
                      useOverlay,
                      address,
                      value,
                      dapClient
                    )
                  } yield response
              }
          }
        }
    }

  private def writeTypedField(
      services: List[IrService],
      engine: OverlayEngine,
      decodeType: String,
      segments: List[String],
      useOverlay: Boolean,
      address: Long,
      value: Json,
      dapClient: DapClient
  ): IO[Response[IO]] = {
    val typeIndex = TypeOverlay.buildTypeIndex(services)
    val shapeIdOpt =
      try {
        Some(
          if (decodeType.contains("#")) ShapeId.from(decodeType)
          else TypeOverlay.normalizeShapeId(decodeType)
        )
      } catch {
        case _: IllegalArgumentException => None
      }
    val owningService = shapeIdOpt
      .flatMap { shapeId =>
        services.find(svc => TypeOverlay.buildTypeIndex(List(svc)).contains(shapeId))
      }
      .orElse(services.headOption)
    val wordSize = owningService.flatMap(_.wordSizeBits)
    val endian = owningService.map(_.defaultEndian).getOrElse(IrEndian.Big)
    val rootTypeOpt = shapeIdOpt.flatMap { shapeId =>
      typeIndex.get(shapeId).orElse {
        typeIndex.collectFirst {
          case (id, t) if id.getName == decodeType || id.toString == decodeType => t
        }
      }
    }

    rootTypeOpt match {
      case None =>
        BadRequest(
          Json.obj(
            "command" -> Json.fromString("writeMemory"),
            "success" -> Json.False,
            "message" -> Json.fromString(s"Unknown decodeType: $decodeType")
          )
        )
      case Some(rootType) =>
        val resolvedRoot =
          if (useOverlay)
            TypeOverlay
              .rewriteType(rootType, engine.document, typeIndex, wordSize)
              .fold(_ => rootType, identity)
          else rootType
        JsonMemoryEncoder.resolveLeaf(
          resolvedRoot,
          segments.filterNot(_.startsWith("_")),
          wordSize
        ) match {
          case Left(err) =>
            BadRequest(
              Json.obj(
                "command" -> Json.fromString("writeMemory"),
                "success" -> Json.False,
                "message" -> Json.fromString(err)
              )
            )
          case Right((leafType, member, _relativeOffset)) =>
            val _ = _relativeOffset
            val memberEndian = member.endianOverride.getOrElse(endian)
            JsonMemoryEncoder.encode(leafType, value, memberEndian, wordSize, Some(member)) match {
              case Left(err) =>
                BadRequest(
                  Json.obj(
                    "command" -> Json.fromString("writeMemory"),
                    "success" -> Json.False,
                    "message" -> Json.fromString(err)
                  )
                )
              case Right(bytes) =>
                val data = Base64.getEncoder.encodeToString(bytes)
                dapClient.writeMemory(address, data).flatMap {
                  case Left(error) =>
                    InternalServerError(
                      Json.obj(
                        "command" -> Json.fromString("writeMemory"),
                        "success" -> Json.False,
                        "message" -> Json.fromString(error)
                      )
                    )
                  case Right(written) =>
                    Ok(
                      Json.obj(
                        "command" -> Json.fromString("writeMemory"),
                        "success" -> Json.True,
                        "body" -> Json.obj(
                          "bytesWritten" -> Json.fromInt(written),
                          "address" -> Json.fromString(f"0x$address%x")
                        )
                      )
                    )
                }
            }
        }
    }
  }

  private def parseHexAddress(raw: String): Option[Long] = {
    val hex = raw.trim.toLowerCase.stripPrefix("0x")
    if (hex.isEmpty || !hex.forall(c => c.isDigit || (c >= 'a' && c <= 'f'))) None
    else
      try Some(java.lang.Long.parseUnsignedLong(hex, 16))
      catch { case _: NumberFormatException => None }
  }
}
