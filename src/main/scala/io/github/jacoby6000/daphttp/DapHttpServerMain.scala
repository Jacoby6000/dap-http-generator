package io.github.jacoby6000.daphttp

import cats.effect.{ExitCode, IO, IOApp, Ref}
import io.circe.Json
import io.circe.syntax._
import com.comcast.ip4s.{Host, Port}
import org.http4s.Method.GET
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.HttpRoutes
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.shapes.{MemberShape, OperationShape, ServiceShape, ShapeId, ShapeType, StructureShape}

import java.io.{BufferedInputStream, BufferedOutputStream}
import java.net.Socket
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.Base64
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.concurrent.duration._
import scala.util.Try

object DapHttpServerMain extends IOApp {
  private final case class Config(
    smithyPaths: List[Path],
    dapHost: String,
    dapPort: Int,
    bindHost: String,
    bindPort: Int,
    watch: Boolean
  )

  final case class ReadPlan(path: String, address: Long, sizeBytes: Int)
  final case class RoutePlan(path: String, reads: List[ReadPlan])

  private val DapStructTrait = ShapeId.from("com.jacoby6000.daphttp#dapStruct")
  private val BitmaskTrait = ShapeId.from("com.jacoby6000.daphttp#bitmask")
  private val SizeTrait = ShapeId.from("com.jacoby6000.daphttp#size")
  private val PaddingTrait = ShapeId.from("com.jacoby6000.daphttp#padding")
  private val PointerTrait = ShapeId.from("com.jacoby6000.daphttp#pointer")
  private val ArrayTrait = ShapeId.from("com.jacoby6000.daphttp#array")
  private val LengthTrait = ShapeId.from("com.jacoby6000.daphttp#length")
  private val CStringTrait = ShapeId.from("com.jacoby6000.daphttp#cString")
  private val WordSizeTrait = ShapeId.from("com.jacoby6000.daphttp#wordSize")
  private val StaticAddressTrait = ShapeId.from("com.jacoby6000.daphttp#staticAddress")
  private val U8Trait = ShapeId.from("com.jacoby6000.daphttp#u8")
  private val S8Trait = ShapeId.from("com.jacoby6000.daphttp#s8")
  private val U16Trait = ShapeId.from("com.jacoby6000.daphttp#u16")
  private val S16Trait = ShapeId.from("com.jacoby6000.daphttp#s16")
  private val U32Trait = ShapeId.from("com.jacoby6000.daphttp#u32")
  private val S32Trait = ShapeId.from("com.jacoby6000.daphttp#s32")
  private val BytesShape = ShapeId.from("com.jacoby6000.daphttp#Bytes")
  private val BitsShape = ShapeId.from("com.jacoby6000.daphttp#Bits")

  override def run(args: List[String]): IO[ExitCode] = {
    parseArgs(args) match {
      case Left(error) =>
        IO.println(error).as(ExitCode.Error)
      case Right(config) =>
        for {
          plansRef <- Ref.of[IO, Either[List[String], Map[String, RoutePlan]]](loadPlans(config.smithyPaths))
          _ <- watchSmithySources(config, plansRef)
          dapClient = new SocketDapClient(config.dapHost, config.dapPort)
          app = routes(plansRef, dapClient).orNotFound
          exit <- EmberServerBuilder
            .default[IO]
            .withHost(Host.fromString(config.bindHost).getOrElse(Host.fromString("0.0.0.0").get))
            .withPort(Port.fromInt(config.bindPort).getOrElse(Port.fromInt(8080).get))
            .withHttpApp(app)
            .build
            .use(_ => IO.never)
            .as(ExitCode.Success)
        } yield exit
    }
  }

  private def parseArgs(args: List[String]): Either[String, Config] = {
    val values = args.flatMap { arg =>
      arg.split("=", 2).toList match {
        case key :: value :: Nil if key.startsWith("--") => Some(key.drop(2) -> value)
        case _                                            => None
      }
    }.toMap

    val smithyPaths = values
      .get("smithy")
      .map(_.split(",").toList.filter(_.nonEmpty).map(Paths.get(_)))
      .getOrElse(Nil)

    if (smithyPaths.isEmpty) {
      Left("Missing required --smithy=/path/a,/path/b argument.")
    } else {
      Right(
        Config(
          smithyPaths = smithyPaths,
          dapHost = values.getOrElse("dapHost", "127.0.0.1"),
          dapPort = values.get("dapPort").flatMap(v => Try(v.toInt).toOption).getOrElse(4711),
          bindHost = values.getOrElse("bindHost", "0.0.0.0"),
          bindPort = values.get("bindPort").flatMap(v => Try(v.toInt).toOption).getOrElse(8080),
          watch = values.get("watch").forall(_.toBooleanOption.getOrElse(true))
        )
      )
    }
  }

  private def routes(plansRef: Ref[IO, Either[List[String], Map[String, RoutePlan]]], dapClient: DapClient): HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case GET -> Root / "health" =>
        Ok(Json.obj("status" -> Json.fromString("ok")))

      case GET -> Root / "routes" =>
        plansRef.get.flatMap {
          case Left(errors) =>
            Ok(Json.obj("errors" -> errors.asJson))
          case Right(routesMap) =>
            Ok(Json.obj("routes" -> routesMap.keys.toList.sorted.asJson))
        }

      case request @ GET -> _ =>
        val routePath = request.uri.path.renderString
        plansRef.get.flatMap {
          case Left(errors) =>
            InternalServerError(Json.obj("errors" -> errors.asJson))
          case Right(routesMap) =>
            routesMap.get(routePath) match {
              case None => NotFound(Json.obj("error" -> Json.fromString(s"No route generated for $routePath")))
              case Some(routePlan) =>
                routePlan.reads.foldLeft(IO.pure(List.empty[Json])) { (accIO, readPlan) =>
                  for {
                    acc <- accIO
                    read <- dapClient.readMemory(readPlan.address, readPlan.sizeBytes)
                  } yield {
                    val readJson = read match {
                      case Right(data) =>
                        Json.obj(
                          "path" -> Json.fromString(readPlan.path),
                          "address" -> Json.fromString(f"0x${readPlan.address}%x"),
                          "bytes" -> Json.fromInt(readPlan.sizeBytes),
                          "data" -> Json.fromString(data)
                        )
                      case Left(error) =>
                        Json.obj(
                          "path" -> Json.fromString(readPlan.path),
                          "address" -> Json.fromString(f"0x${readPlan.address}%x"),
                          "bytes" -> Json.fromInt(readPlan.sizeBytes),
                          "error" -> Json.fromString(error)
                        )
                    }
                    acc :+ readJson
                  }
                }.flatMap { reads =>
                  Ok(
                    Json.obj(
                      "route" -> Json.fromString(routePlan.path),
                      "reads" -> reads.asJson
                    )
                  )
                }
            }
        }
    }

  private def loadPlans(smithyPaths: List[Path]): Either[List[String], Map[String, RoutePlan]] = {
    loadModel(smithyPaths).flatMap(buildRoutePlansFromModel)
  }

  private def loadModel(smithyPaths: List[Path]): Either[List[String], Model] = {
    val smithyFiles = smithyPaths.flatMap(collectSmithyFiles).distinct
    val assembler = Model.assembler()
    smithyFiles.foreach(path => assembler.addImport(path.toString))
    val result = assembler.assemble()
    if (result.isBroken) {
      Left(result.getValidationEvents.asScala.map(_.toString).toList)
    } else {
      Right(result.unwrap())
    }
  }

  private def collectSmithyFiles(path: Path): List[Path] = {
    if (!Files.exists(path)) {
      Nil
    } else if (Files.isRegularFile(path) && path.toString.endsWith(".smithy")) {
      List(path)
    } else if (Files.isDirectory(path)) {
      val stream = Files.walk(path)
      try {
        stream.iterator().asScala.filter(p => Files.isRegularFile(p) && p.toString.endsWith(".smithy")).toList
      } finally {
        stream.close()
      }
    } else {
      Nil
    }
  }

  def buildRoutePlansFromModel(model: Model): Either[List[String], Map[String, RoutePlan]] = {
    val errors = ListBuffer.empty[String]

    val services = model.shapes(classOf[ServiceShape]).iterator().asScala.toList
    val wordSize = services.flatMap(service => intTraitValue(service, WordSizeTrait)).headOption

    services.foreach { service =>
      if (intTraitValue(service, WordSizeTrait).isEmpty) {
        errors += s"${service.getId}: Services must declare @wordSize."
      }
    }

    val routePlans = services.flatMap { service =>
      service.getOperations.asScala.toList.flatMap { operationId =>
        val operation = model.expectShape(operationId, classOf[OperationShape])
        operation.getOutput.toScala.map { outputId =>
          val routePath = s"/${service.getId.getName}/${operation.getId.getName}"
          val outputShape = model.expectShape(outputId, classOf[StructureShape])
          val reads = collectReadsForStructure(model, outputShape, None, routePath, wordSize, errors)
          routePath -> RoutePlan(routePath, reads)
        }
      }
    }.toMap

    if (errors.nonEmpty) Left(errors.toList.distinct) else Right(routePlans)
  }

  private def collectReadsForStructure(
    model: Model,
    structure: StructureShape,
    baseAddress: Option[Long],
    pathPrefix: String,
    wordSize: Option[Int],
    errors: ListBuffer[String]
  ): List[ReadPlan] = {
    val isDapShape = structure.hasTrait(DapStructTrait) || structure.hasTrait(BitmaskTrait)
    if (isDapShape) {
      baseAddress match {
        case None =>
          errors += s"${structure.getId}: DAP-backed structures must be reachable from @staticAddress members."
          Nil
        case Some(address) =>
          structureSizeBytes(model, structure, wordSize, errors) match {
            case Some(sizeBytes) => List(ReadPlan(pathPrefix, address, sizeBytes))
            case None            => Nil
          }
      }
    } else {
      structure.members().asScala.toList.flatMap { member =>
        val memberPath = s"$pathPrefix.${member.getMemberName}"
        val memberAddress = staticAddress(member).orElse {
          errors += s"${member.getId}: Members of non-DAP structures must declare @staticAddress."
          None
        }

        val target = model.expectShape(member.getTarget)
        target.getType match {
          case ShapeType.STRUCTURE =>
            collectReadsForStructure(
              model,
              target.asInstanceOf[StructureShape],
              memberAddress,
              memberPath,
              wordSize,
              errors
            )
          case _ =>
            memberAddress.flatMap(address => memberSizeBytes(model, member, wordSize, errors).map(ReadPlan(memberPath, address, _))).toList
        }
      }
    }
  }

  private def structureSizeBytes(
    model: Model,
    structure: StructureShape,
    wordSize: Option[Int],
    errors: ListBuffer[String]
  ): Option[Int] = {
    intTraitValue(structure, SizeTrait).map { raw =>
      if (structure.hasTrait(BitmaskTrait)) math.ceil(raw.toDouble / 8d).toInt else raw
    }.orElse {
      val bits = structure.members().asScala.toList.flatMap(member => memberBitWidth(model, member, wordSize, errors))
      if (bits.isEmpty) {
        errors += s"${structure.getId}: Unable to infer read width; add @size."
        None
      } else {
        Some(math.ceil(bits.sum.toDouble / 8d).toInt)
      }
    }
  }

  private def memberSizeBytes(
    model: Model,
    member: MemberShape,
    wordSize: Option[Int],
    errors: ListBuffer[String]
  ): Option[Int] = {
    memberBitWidth(model, member, wordSize, errors).map(bits => math.ceil(bits.toDouble / 8d).toInt)
  }

  private def memberBitWidth(
    model: Model,
    member: MemberShape,
    wordSize: Option[Int],
    errors: ListBuffer[String]
  ): Option[Int] = {
    if (member.hasTrait(PointerTrait)) {
      return wordSize.orElse {
        errors += s"${member.getId}: Pointer members require service @wordSize."
        None
      }
    }

    val target = model.expectShape(member.getTarget)
    cStringBytes(member).map(_ * 8).orElse {
      if (member.hasTrait(U8Trait) || member.hasTrait(S8Trait)) {
        Some(8)
      } else if (member.hasTrait(U16Trait) || member.hasTrait(S16Trait)) {
        Some(16)
      } else if (member.hasTrait(U32Trait) || member.hasTrait(S32Trait)) {
        Some(32)
      } else {
      target.getType match {
        case ShapeType.BOOLEAN => Some(1)
        case ShapeType.BYTE    => Some(8)
        case ShapeType.SHORT   => Some(16)
        case ShapeType.INTEGER => Some(32)
        case ShapeType.LONG    => wordSize.orElse(Some(64))
        case ShapeType.LIST =>
          val listShape = target
          val isArray = member.hasTrait(ArrayTrait)
          val isPointerArray = isArray && member.hasTrait(PointerTrait)
          if (isArray && !isPointerArray) {
            intTraitValue(member, LengthTrait).flatMap { length =>
              listElementBitWidth(model, listShape.getId, listShape.asInstanceOf[software.amazon.smithy.model.shapes.ListShape].getMember.getTarget, wordSize)
                .map(_ * length)
            }.orElse {
              errors += s"${member.getId}: Non-pointer arrays must declare @length."
              None
            }
          } else {
            intTraitValue(member, PaddingTrait).flatMap { repeats =>
              listShape.getId match {
                case id if id == BytesShape => Some(repeats * 8)
                case id if id == BitsShape  => Some(repeats)
                case _ =>
                  errors += s"${member.getId}: @padding is only supported for Bytes/Bits list shapes."
                  None
              }
            }
          }
        case _ => None
      }
      }
    }
  }

  private def listElementBitWidth(model: Model, listShapeId: ShapeId, memberTarget: ShapeId, wordSize: Option[Int]): Option[Int] = {
    if (listShapeId == BytesShape) {
      Some(8)
    } else if (listShapeId == BitsShape) {
      Some(1)
    } else {
      model.expectShape(memberTarget).getType match {
        case ShapeType.BOOLEAN => Some(1)
        case ShapeType.BYTE    => Some(8)
        case ShapeType.SHORT   => Some(16)
        case ShapeType.INTEGER => Some(32)
        case ShapeType.LONG    => wordSize.orElse(Some(64))
        case _                 => None
      }
    }
  }

  private def cStringBytes(member: MemberShape): Option[Int] = {
    member.findTrait(CStringTrait).toScala.flatMap { rawTrait =>
      val node = rawTrait.toNode
      if (node.isNumberNode) Some(node.expectNumberNode.getValue.intValue())
      else if (node.isObjectNode) Some(node.expectObjectNode.expectNumberMember("bytes").getValue.intValue())
      else None
    }
  }

  private def intTraitValue(shape: software.amazon.smithy.model.shapes.Shape, traitId: ShapeId): Option[Int] = {
    shape.findTrait(traitId).toScala.flatMap { rawTrait =>
      val node = rawTrait.toNode
      if (node.isNumberNode) Some(node.expectNumberNode.getValue.intValue()) else None
    }
  }

  private def staticAddress(member: MemberShape): Option[Long] = {
    member.findTrait(StaticAddressTrait).toScala.flatMap { rawTrait =>
      val node = rawTrait.toNode
      if (node.isStringNode) {
        val value = node.expectStringNode.getValue.trim
        if (value.startsWith("0x") || value.startsWith("0X")) {
          Try(java.lang.Long.parseUnsignedLong(value.drop(2), 16)).toOption
        } else {
          Try(value.toLong).toOption
        }
      } else if (node.isNumberNode) {
        Some(node.expectNumberNode.getValue.longValue())
      } else {
        None
      }
    }
  }

  private def watchSmithySources(
    config: Config,
    plansRef: Ref[IO, Either[List[String], Map[String, RoutePlan]]]
  ): IO[Unit] = {
    if (!config.watch) {
      IO.unit
    } else {
      def newestTimestamp(paths: List[Path]): Long =
        paths
          .flatMap(collectSmithyFiles)
          .flatMap(path => Try(Files.getLastModifiedTime(path).toMillis).toOption)
          .sorted
          .lastOption
          .getOrElse(0L)

      def loop(lastSeen: Long): IO[Unit] =
        IO.sleep(2.seconds) *> IO.blocking(newestTimestamp(config.smithyPaths)).flatMap {
          newest =>
            if (newest > lastSeen) {
              plansRef.set(loadPlans(config.smithyPaths)) *> loop(newest)
            } else {
              loop(lastSeen)
            }
        }
      IO.blocking(newestTimestamp(config.smithyPaths)).flatMap(ts => loop(ts).start.void)
    }
  }

  private trait DapClient {
    def readMemory(address: Long, sizeBytes: Int): IO[Either[String, String]]
  }

  private final class SocketDapClient(host: String, port: Int) extends DapClient {
    override def readMemory(address: Long, sizeBytes: Int): IO[Either[String, String]] =
      IO.blocking {
        val socket = new Socket(host, port)
        socket.setSoTimeout(5000)
        val out = new BufferedOutputStream(socket.getOutputStream)
        val in = new BufferedInputStream(socket.getInputStream)

        val request =
          Json.obj(
            "seq" -> Json.fromInt(1),
            "type" -> Json.fromString("request"),
            "command" -> Json.fromString("readMemory"),
            "arguments" -> Json.obj(
              "memoryReference" -> Json.fromString(f"0x$address%x"),
              "count" -> Json.fromInt(sizeBytes)
            )
          ).noSpaces

        val payload = request.getBytes(StandardCharsets.UTF_8)
        out.write(s"Content-Length: ${payload.length}\r\n\r\n".getBytes(StandardCharsets.UTF_8))
        out.write(payload)
        out.flush()

        val contentLength = readContentLength(in)
        val body = readBody(in, contentLength)

        socket.close()

        io.circe.parser.parse(body).toOption match {
          case Some(json) if json.hcursor.downField("success").as[Boolean].getOrElse(false) =>
            val value = json.hcursor
              .downField("body")
              .downField("data")
              .as[String]
              .toOption
              .getOrElse(Base64.getEncoder.encodeToString(body.getBytes(StandardCharsets.UTF_8)))
            Right(value)
          case Some(json) =>
            Left(json.hcursor.downField("message").as[String].toOption.getOrElse("DAP readMemory failed"))
          case None =>
            Left("Failed to parse DAP response payload.")
        }
      }.handleError(error => Left(error.getMessage))

    private def readContentLength(in: BufferedInputStream): Int = {
      var contentLength = 0
      var line = readLine(in)
      while (line.nonEmpty) {
        val lower = line.toLowerCase
        if (lower.startsWith("content-length:")) {
          contentLength = lower.stripPrefix("content-length:").trim.toInt
        }
        line = readLine(in)
      }
      contentLength
    }

    private def readBody(in: BufferedInputStream, length: Int): String = {
      val buffer = new Array[Byte](length)
      var read = 0
      while (read < length) {
        val bytesRead = in.read(buffer, read, length - read)
        if (bytesRead == -1) throw new IllegalStateException("Unexpected EOF while reading DAP response body.")
        read += bytesRead
      }
      new String(buffer, StandardCharsets.UTF_8)
    }

    private def readLine(in: BufferedInputStream): String = {
      val buffer = new StringBuilder
      var current = in.read()
      var previous = -1
      while (current != -1 && !(previous == '\r' && current == '\n')) {
        if (current != '\r') buffer.append(current.toChar)
        previous = current
        current = in.read()
      }
      buffer.toString()
    }
  }
}
