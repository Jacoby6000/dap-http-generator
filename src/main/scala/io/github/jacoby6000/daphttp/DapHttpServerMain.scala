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
import software.amazon.smithy.model.shapes.{
  ListShape,
  MapShape,
  MemberShape,
  OperationShape,
  ServiceShape,
  Shape,
  ShapeId,
  ShapeType,
  StructureShape,
  UnionShape
}

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
  private final case class IrService(name: String, wordSizeBits: Option[Int], operations: List[IrOperation])
  private final case class IrOperation(name: String, routePath: String, output: IrType.Struct)
  private final case class IrMember(
    id: ShapeId,
    name: String,
    target: IrType,
    staticAddress: Option[Long],
    cStringBytes: Option[Int],
    paddingRepeats: Option[Int],
    isPointer: Boolean,
    isArray: Boolean,
    arrayLength: Option[Int],
    primitiveOverride: Option[IrPrimitive]
  )
  private sealed trait IrPrimitive
  private object IrPrimitive {
    case object U8 extends IrPrimitive
    case object S8 extends IrPrimitive
    case object U16 extends IrPrimitive
    case object S16 extends IrPrimitive
    case object U32 extends IrPrimitive
    case object S32 extends IrPrimitive
    case object Bool extends IrPrimitive
    case object LongWord extends IrPrimitive
  }
  private sealed trait IrType
  private object IrType {
    final case class Struct(
      id: ShapeId,
      members: List[IrMember],
      isDapStruct: Boolean,
      isBitmask: Boolean,
      declaredSizeBits: Option[Int]
    ) extends IrType
    final case class Union(id: ShapeId, members: List[IrMember]) extends IrType
    final case class ListType(id: ShapeId, element: IrType, bytesAlias: Boolean, bitsAlias: Boolean) extends IrType
    final case class MapType(id: ShapeId, key: IrType, value: IrType) extends IrType
    final case class Primitive(kind: IrPrimitive) extends IrType
    final case class Ref(id: ShapeId) extends IrType
  }

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
    buildIrFromModel(model).flatMap(compileRoutePlansFromIr)
  }

  private def buildIrFromModel(model: Model): Either[List[String], List[IrService]] = {
    val errors = ListBuffer.empty[String]
    val services = model.shapes(classOf[ServiceShape]).iterator().asScala.toList

    def buildIrType(shapeId: ShapeId, stack: Set[ShapeId]): IrType = {
      if (stack.contains(shapeId)) {
        IrType.Ref(shapeId)
      } else {
        val shape = model.expectShape(shapeId)
        shape.getType match {
          case ShapeType.STRUCTURE =>
            val structure = shape.asInstanceOf[StructureShape]
            IrType.Struct(
              id = structure.getId,
              members = structure.members().asScala.toList.map { member =>
                IrMember(
                  id = member.getId,
                  name = member.getMemberName,
                  target = buildIrType(member.getTarget, stack + shapeId),
                  staticAddress = staticAddress(member),
                  cStringBytes = cStringBytes(member),
                  paddingRepeats = intTraitValue(member, PaddingTrait),
                  isPointer = member.hasTrait(PointerTrait),
                  isArray = member.hasTrait(ArrayTrait),
                  arrayLength = intTraitValue(member, LengthTrait),
                  primitiveOverride = memberPrimitiveOverride(member)
                )
              },
              isDapStruct = structure.hasTrait(DapStructTrait),
              isBitmask = structure.hasTrait(BitmaskTrait),
              declaredSizeBits = intTraitValue(structure, SizeTrait)
            )
          case ShapeType.UNION =>
            val union = shape.asInstanceOf[UnionShape]
            IrType.Union(
              id = union.getId,
              members = union.members().asScala.toList.map { member =>
                IrMember(
                  id = member.getId,
                  name = member.getMemberName,
                  target = buildIrType(member.getTarget, stack + shapeId),
                  staticAddress = staticAddress(member),
                  cStringBytes = cStringBytes(member),
                  paddingRepeats = intTraitValue(member, PaddingTrait),
                  isPointer = member.hasTrait(PointerTrait),
                  isArray = member.hasTrait(ArrayTrait),
                  arrayLength = intTraitValue(member, LengthTrait),
                  primitiveOverride = memberPrimitiveOverride(member)
                )
              }
            )
          case ShapeType.LIST =>
            val list = shape.asInstanceOf[ListShape]
            IrType.ListType(
              id = list.getId,
              element = buildIrType(list.getMember.getTarget, stack + shapeId),
              bytesAlias = list.getId == BytesShape,
              bitsAlias = list.getId == BitsShape
            )
          case ShapeType.MAP =>
            val mapShape = shape.asInstanceOf[MapShape]
            IrType.MapType(
              id = mapShape.getId,
              key = buildIrType(mapShape.getKey.getTarget, stack + shapeId),
              value = buildIrType(mapShape.getValue.getTarget, stack + shapeId)
            )
          case primitiveType =>
            primitiveForShapeType(primitiveType)
              .map(IrType.Primitive.apply)
              .getOrElse(IrType.Ref(shapeId))
        }
      }
    }

    val irServices = services.map { service =>
      val wordSize = intTraitValue(service, WordSizeTrait)
      if (wordSize.isEmpty) {
        errors += s"${service.getId}: Services must declare @wordSize."
      }
      val operations = service.getOperations.asScala.toList.flatMap { operationId =>
        val operation = model.expectShape(operationId, classOf[OperationShape])
        operation.getOutput.toScala.flatMap { outputId =>
          val outputShape = buildIrType(outputId, Set.empty)
          outputShape match {
            case outputStruct: IrType.Struct =>
              val routePath = s"/${service.getId.getName}/${operation.getId.getName}"
              Some(IrOperation(operation.getId.getName, routePath, outputStruct))
            case _ =>
              errors += s"${operation.getId}: Operation output must be a structure."
              None
          }
        }
      }
      IrService(service.getId.getName, wordSize, operations)
    }

    if (errors.nonEmpty) Left(errors.toList.distinct) else Right(irServices)
  }

  private def compileRoutePlansFromIr(irServices: List[IrService]): Either[List[String], Map[String, RoutePlan]] = {
    val errors = ListBuffer.empty[String]

    irServices.foreach { service =>
      if (service.wordSizeBits.isEmpty) {
        errors += s"${service.name}: Services must declare @wordSize."
      }
    }

    val routePlans = irServices.flatMap { service =>
      service.operations.map { operation =>
        val reads = collectReadsForType(operation.output, None, operation.routePath, service.wordSizeBits, errors)
        operation.routePath -> RoutePlan(operation.routePath, reads)
      }
    }.toMap

    if (errors.nonEmpty) Left(errors.toList.distinct) else Right(routePlans)
  }

  private def collectReadsForType(
    irType: IrType,
    baseAddress: Option[Long],
    pathPrefix: String,
    wordSize: Option[Int],
    errors: ListBuffer[String]
  ): List[ReadPlan] = {
    irType match {
      case struct: IrType.Struct =>
        val isDapShape = struct.isDapStruct || struct.isBitmask
        if (isDapShape) {
          baseAddress match {
            case None =>
              errors += s"${struct.id}: DAP-backed structures must be reachable from @staticAddress members."
              Nil
            case Some(address) =>
              structureSizeBytes(struct, wordSize, errors) match {
                case Some(sizeBytes) => List(ReadPlan(pathPrefix, address, sizeBytes))
                case None            => Nil
              }
          }
        } else {
          struct.members.flatMap { member =>
            val memberPath = s"$pathPrefix.${member.name}"
            val memberAddress = member.staticAddress.orElse {
              errors += s"${member.id}: Members of non-DAP structures must declare @staticAddress."
              None
            }
            member.target match {
              case nestedStruct: IrType.Struct =>
                collectReadsForType(nestedStruct, memberAddress, memberPath, wordSize, errors)
              case _ =>
                memberAddress.flatMap(address => memberSizeBytes(member, wordSize, errors).map(ReadPlan(memberPath, address, _))).toList
            }
          }
        }
      case union: IrType.Union =>
        errors += s"${union.id}: Union outputs are modeled in IR but not yet readable from static layouts."
        Nil
      case mapType: IrType.MapType =>
        errors += s"${mapType.id}: Map outputs are modeled in IR but not yet readable from static layouts."
        Nil
      case listType: IrType.ListType =>
        errors += s"${listType.id}: Top-level list outputs are modeled in IR but must be wrapped in a structure."
        Nil
      case _: IrType.Primitive =>
        errors += s"$pathPrefix: Primitive outputs are modeled in IR but must be wrapped in a structure."
        Nil
      case ref: IrType.Ref =>
        errors += s"${ref.id}: Unsupported shape for route planning."
        Nil
    }
  }

  private def structureSizeBytes(
    structure: IrType.Struct,
    wordSize: Option[Int],
    errors: ListBuffer[String]
  ): Option[Int] = {
    structure.declaredSizeBits.map { raw =>
      if (structure.isBitmask) math.ceil(raw.toDouble / 8d).toInt else raw
    }.orElse {
      val bits = structure.members.flatMap(member => memberBitWidth(member, wordSize, errors))
      if (bits.isEmpty) {
        errors += s"${structure.id}: Unable to infer read width; add @size."
        None
      } else {
        Some(math.ceil(bits.sum.toDouble / 8d).toInt)
      }
    }
  }

  private def memberSizeBytes(
    member: IrMember,
    wordSize: Option[Int],
    errors: ListBuffer[String]
  ): Option[Int] = {
    memberBitWidth(member, wordSize, errors).map(bits => math.ceil(bits.toDouble / 8d).toInt)
  }

  private def memberBitWidth(
    member: IrMember,
    wordSize: Option[Int],
    errors: ListBuffer[String]
  ): Option[Int] = {
    if (member.isPointer) {
      return wordSize.orElse {
        errors += s"${member.id}: Pointer members require service @wordSize."
        None
      }
    }

    member.cStringBytes.map(_ * 8).orElse {
      member.primitiveOverride.flatMap(bitsForPrimitive(_, wordSize)).orElse {
        member.target match {
          case IrType.Primitive(kind) =>
            bitsForPrimitive(kind, wordSize)
          case listType: IrType.ListType =>
            listBitWidth(member, listType, wordSize, errors)
          case nestedStruct: IrType.Struct =>
            structureSizeBytes(nestedStruct, wordSize, errors).map(_ * 8)
          case _ =>
            None
        }
      }
    }
  }

  private def listBitWidth(
    member: IrMember,
    listType: IrType.ListType,
    wordSize: Option[Int],
    errors: ListBuffer[String]
  ): Option[Int] = {
    val isPointerArray = member.isArray && member.isPointer
    if (member.isArray && !isPointerArray) {
      member.arrayLength.flatMap { length =>
        listElementBitWidth(listType.element, wordSize).map(_ * length)
      }.orElse {
        errors += s"${member.id}: Non-pointer arrays must declare @length."
        None
      }
    } else {
      member.paddingRepeats.flatMap { repeats =>
        if (listType.bytesAlias) {
          Some(repeats * 8)
        } else if (listType.bitsAlias) {
          Some(repeats)
        } else {
          errors += s"${member.id}: @padding is only supported for Bytes/Bits list shapes."
          None
        }
      }
    }
  }

  private def listElementBitWidth(elementType: IrType, wordSize: Option[Int]): Option[Int] = {
    elementType match {
      case IrType.Primitive(kind) => bitsForPrimitive(kind, wordSize)
      case nestedStruct: IrType.Struct =>
        structureSizeBytes(nestedStruct, wordSize, ListBuffer.empty).map(_ * 8)
      case _ =>
        None
    }
  }

  private def bitsForPrimitive(kind: IrPrimitive, wordSize: Option[Int]): Option[Int] = {
    kind match {
      case IrPrimitive.Bool        => Some(1)
      case IrPrimitive.U8          => Some(8)
      case IrPrimitive.S8          => Some(8)
      case IrPrimitive.U16         => Some(16)
      case IrPrimitive.S16         => Some(16)
      case IrPrimitive.U32         => Some(32)
      case IrPrimitive.S32         => Some(32)
      case IrPrimitive.LongWord    => wordSize.orElse(Some(64))
    }
  }

  private def primitiveForShapeType(shapeType: ShapeType): Option[IrPrimitive] = {
    shapeType match {
      case ShapeType.BOOLEAN => Some(IrPrimitive.Bool)
      case ShapeType.BYTE    => Some(IrPrimitive.S8)
      case ShapeType.SHORT   => Some(IrPrimitive.S16)
      case ShapeType.INTEGER => Some(IrPrimitive.S32)
      case ShapeType.LONG    => Some(IrPrimitive.LongWord)
      case _                 => None
    }
  }

  private def memberPrimitiveOverride(member: MemberShape): Option[IrPrimitive] = {
    if (member.hasTrait(U8Trait)) {
      Some(IrPrimitive.U8)
    } else if (member.hasTrait(S8Trait)) {
      Some(IrPrimitive.S8)
    } else if (member.hasTrait(U16Trait)) {
      Some(IrPrimitive.U16)
    } else if (member.hasTrait(S16Trait)) {
      Some(IrPrimitive.S16)
    } else if (member.hasTrait(U32Trait)) {
      Some(IrPrimitive.U32)
    } else if (member.hasTrait(S32Trait)) {
      Some(IrPrimitive.S32)
    } else {
      None
    }
  }

  private def cStringBytes(member: MemberShape): Option[Int] = {
    member.findTrait(CStringTrait).toScala.flatMap { rawTrait =>
      val node = rawTrait.toNode
      if (node.isNumberNode) {
        Some(node.expectNumberNode.getValue.intValue())
      } else if (node.isObjectNode) {
        Some(node.expectObjectNode.expectNumberMember("bytes").getValue.intValue())
      } else {
        None
      }
    }
  }

  private def intTraitValue(shape: Shape, traitId: ShapeId): Option[Int] = {
    shape.findTrait(traitId).toScala.flatMap { rawTrait =>
      val node = rawTrait.toNode
      if (node.isNumberNode) {
        Some(node.expectNumberNode.getValue.intValue())
      } else {
        None
      }
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
