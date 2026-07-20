package io.github.jacoby6000.daphttp

import cats.effect.IO
import cats.effect.Ref
import com.comcast.ip4s.Host
import com.comcast.ip4s.Port
import io.circe.Json
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.scalatest.funsuite.AnyFunSuite
import software.amazon.smithy.model.Model

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.net.ServerSocket
import java.net.Socket
import java.net.SocketException
import java.net.URI
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse
import java.nio.charset.StandardCharsets
import java.util.Base64

class DapHttpIntegrationSpec extends AnyFunSuite {
  private final class DummyDapServer(payloads: Map[(Long, Int), Array[Byte]]) {
    @volatile private var running = true
    private val server = new ServerSocket(0)
    private val acceptThread = new Thread(() => run(), "dummy-dap-server")
    acceptThread.setDaemon(true)
    acceptThread.start()

    val port: Int = server.getLocalPort

    def close(): Unit = {
      running = false
      server.close()
      acceptThread.join(1000L)
    }

    private def run(): Unit = {
      while (running) {
        try {
          val socket = server.accept()
          handle(socket)
        } catch {
          case _: SocketException if !running => ()
        }
      }
    }

    private def handle(socket: Socket): Unit = {
      val in = new BufferedInputStream(socket.getInputStream)
      val out = new BufferedOutputStream(socket.getOutputStream)
      val body = readBody(in)
      val cursor = io.circe.parser.parse(body).toOption.map(_.hcursor)
      val memoryReference =
        cursor.flatMap(_.downField("arguments").downField("memoryReference").as[String].toOption)
      val count = cursor.flatMap(_.downField("arguments").downField("count").as[Int].toOption)

      val address = memoryReference.flatMap(parseAddress)
      val data = address.flatMap(addr => count.flatMap(c => payloads.get((addr, c))))
      val responseJson = data match {
        case Some(bytes) =>
          Json.obj(
            "success" -> Json.True,
            "body" -> Json.obj(
              "data" -> Json.fromString(Base64.getEncoder.encodeToString(bytes))
            )
          )
        case None =>
          Json.obj(
            "success" -> Json.False,
            "message" -> Json.fromString("missing payload")
          )
      }
      val responseBody = responseJson.noSpaces.getBytes(StandardCharsets.UTF_8)
      out.write(s"Content-Length: ${responseBody.length}\r\n\r\n".getBytes(StandardCharsets.UTF_8))
      out.write(responseBody)
      out.flush()
      socket.close()
    }

    private def parseAddress(memoryReference: String): Option[Long] = {
      val trimmed = memoryReference.trim
      if (trimmed.startsWith("0x") || trimmed.startsWith("0X")) {
        scala.util.Try(java.lang.Long.parseUnsignedLong(trimmed.drop(2), 16)).toOption
      } else {
        scala.util.Try(trimmed.toLong).toOption
      }
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

    private def readBody(in: BufferedInputStream): String = {
      var contentLength = 0
      var line = readLine(in)
      while (line.nonEmpty) {
        val lower = line.toLowerCase
        if (lower.startsWith("content-length:")) {
          contentLength = lower.stripPrefix("content-length:").trim.toInt
        }
        line = readLine(in)
      }
      val bytes = new Array[Byte](contentLength)
      var offset = 0
      while (offset < contentLength) {
        val read = in.read(bytes, offset, contentLength - offset)
        if (read == -1)
          throw new IllegalStateException("Unexpected EOF while reading request body.")
        offset += read
      }
      new String(bytes, StandardCharsets.UTF_8)
    }
  }

  test("decodes DAP memory payloads through generated http routes") {
    val model = Model
      .assembler()
      .addImport("src/main/smithy/dap-http-traits.smithy")
      .addUnparsedModel(
        "integration.smithy",
        """$version: "2"
          |
          |namespace example
          |
          |use com.jacoby6000.daphttp#dapStruct
          |use com.jacoby6000.daphttp#char
          |use com.jacoby6000.daphttp#pointer
          |use com.jacoby6000.daphttp#size
          |use com.jacoby6000.daphttp#staticAddress
          |use com.jacoby6000.daphttp#u16
          |use com.jacoby6000.daphttp#wordSize
          |use com.jacoby6000.daphttp#endian
          |
          |@wordSize(32)
          |@endian("little")
          |service Api {
          |  version: "1"
          |  operations: [GetLittle16, GetRegs, GetName]
          |}
          |
          |operation GetLittle16 {
          |  output: GetLittle16Output
          |}
          |
          |operation GetRegs {
          |  output: GetRegsOutput
          |}
          |
          |operation GetName {
          |  output: GetNameOutput
          |}
          |
          |structure GetLittle16Output {
          |  @staticAddress("0x1000")
          |  @u16
          |  value: Integer
          |}
          |
          |structure GetRegsOutput {
          |  @staticAddress("0x2000")
          |  regs: Registers
          |}
          |
          |@dapStruct
          |@size(4)
          |structure Registers {
          |  @u16
          |  lo: Integer
          |  @u16
          |  hi: Integer
          |}
          |
          |structure GetNameOutput {
          |  @staticAddress("0x3000")
          |  @pointer
          |  @char
          |  name: Byte
          |}
          |""".stripMargin
      )
      .assemble()
      .unwrap()

    val routePlans = DapHttpServerMain.buildRoutePlansFromModel(model).toOption.get
    val dummyDap = new DummyDapServer(
      Map(
        (0x1000L, 2) -> Array(0x34.toByte, 0x12.toByte),
        (0x2000L, 4) -> Array(0x78.toByte, 0x56.toByte, 0xbc.toByte, 0x9a.toByte),
        (0x3000L, 4) -> Array(0x00.toByte, 0x40.toByte, 0x00.toByte, 0x00.toByte),
        (0x4000L, 1) -> Array('O'.toByte),
        (0x4001L, 1) -> Array('K'.toByte),
        (0x4002L, 1) -> Array(0x00.toByte)
      )
    )

    try {
      val result = (for {
        plansRef <- cats.effect.Resource.eval(
          Ref.of[IO, Either[List[String], Map[String, RoutePlan]]](Right(routePlans))
        )
        server <- EmberServerBuilder
          .default[IO]
          .withHost(Host.fromString("127.0.0.1").get)
          .withPort(Port.fromInt(0).get)
          .withHttpApp(
            DapHttpServerMain
              .routes(plansRef, new SocketDapClient("127.0.0.1", dummyDap.port))
              .orNotFound
          )
          .build
      } yield {
        val port = server.address.getPort
        val client = HttpClient.newHttpClient()
        val little16 = getResponse(client, s"http://127.0.0.1:$port/Api/GetLittle16")
        val regs = getResponse(client, s"http://127.0.0.1:$port/Api/GetRegs")
        val name = getResponse(client, s"http://127.0.0.1:$port/Api/GetName")

        val littleDecoded = little16.hcursor
          .downField("reads")
          .downN(0)
          .downField("decoded")
          .as[Json]
          .toOption
          .get
        val regsDecoded =
          regs.hcursor.downField("reads").downN(0).downField("decoded").as[Json].toOption.get
        val nameDecoded =
          name.hcursor.downField("reads").downN(0).downField("decoded").as[Json].toOption.get

        assert(littleDecoded == Json.fromLong(0x1234L))
        assert(
          regsDecoded == Json.obj("lo" -> Json.fromLong(0x5678L), "hi" -> Json.fromLong(0x9abcL))
        )
        assert(nameDecoded == Json.fromString("OK"))
      }).use(IO.pure(_))

      result.unsafeRunSync()(cats.effect.unsafe.IORuntime.global)
    } finally {
      dummyDap.close()
    }
  }

  private def getResponse(client: HttpClient, url: String): Json = {
    val request = HttpRequest.newBuilder().uri(URI.create(url)).GET().build()
    val response = client.send(request, HttpResponse.BodyHandlers.ofString())
    io.circe.parser.parse(response.body()).toOption.get
  }
}
