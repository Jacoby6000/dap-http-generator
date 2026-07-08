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
      try {
        var keepReading = true
        while (keepReading) {
          val body = readBody(in)
          if (body.isEmpty) {
            keepReading = false
          } else {
            val cursor = io.circe.parser.parse(body).toOption.map(_.hcursor)
            val messageType = cursor.flatMap(_.downField("type").as[String].toOption)
            val command = cursor.flatMap(_.downField("command").as[String].toOption)
            val event = cursor.flatMap(_.downField("event").as[String].toOption)
            val requestSeq = cursor.flatMap(_.downField("seq").as[Int].toOption).getOrElse(0)

            messageType match {
              case Some("event") if event.contains("initialized") =>
                ()
              case Some("request") if command.contains("initialize") =>
                writeResponse(
                  out,
                  requestSeq,
                  "initialize",
                  Json.obj("supportsConfigurationDoneRequest" -> Json.False)
                )
              case Some("request") if command.contains("configurationDone") =>
                writeResponse(out, requestSeq, "configurationDone", Json.obj())
              case Some("request") if command.contains("readMemory") =>
                val memoryReference =
                  cursor.flatMap(
                    _.downField("arguments").downField("memoryReference").as[String].toOption
                  )
                val count =
                  cursor.flatMap(_.downField("arguments").downField("count").as[Int].toOption)
                val address = memoryReference.flatMap(parseAddress)
                val data = address.flatMap(addr => count.flatMap(c => payloads.get((addr, c))))
                data match {
                  case Some(bytes) =>
                    writeResponse(
                      out,
                      requestSeq,
                      "readMemory",
                      Json.obj("data" -> Json.fromString(Base64.getEncoder.encodeToString(bytes)))
                    )
                  case None =>
                    writeFailure(out, requestSeq, "readMemory", "missing payload")
                }
              case Some("request") if command.contains("threads") =>
                writeResponse(
                  out,
                  requestSeq,
                  "threads",
                  Json.obj(
                    "threads" -> Json.arr(
                      Json.obj("id" -> Json.fromInt(1), "name" -> Json.fromString("main"))
                    )
                  )
                )
              case Some("request") if command.contains("continue") =>
                writeResponse(
                  out,
                  requestSeq,
                  "continue",
                  Json.obj("allThreadsContinued" -> Json.True)
                )
              case _ =>
                keepReading = false
            }
          }
        }
      } finally {
        socket.close()
      }
    }

    private def writeResponse(
        out: BufferedOutputStream,
        requestSeq: Int,
        command: String,
        body: Json
    ): Unit = {
      val responseJson = Json.obj(
        "seq" -> Json.fromInt(requestSeq + 1000),
        "type" -> Json.fromString("response"),
        "request_seq" -> Json.fromInt(requestSeq),
        "success" -> Json.True,
        "command" -> Json.fromString(command),
        "body" -> body
      )
      writeFramed(out, responseJson)
    }

    private def writeFailure(
        out: BufferedOutputStream,
        requestSeq: Int,
        command: String,
        message: String
    ): Unit = {
      val responseJson = Json.obj(
        "seq" -> Json.fromInt(requestSeq + 1000),
        "type" -> Json.fromString("response"),
        "request_seq" -> Json.fromInt(requestSeq),
        "success" -> Json.False,
        "command" -> Json.fromString(command),
        "message" -> Json.fromString(message)
      )
      writeFramed(out, responseJson)
    }

    private def writeFramed(out: BufferedOutputStream, json: Json): Unit = {
      val responseBody = json.noSpaces.getBytes(StandardCharsets.UTF_8)
      out.write(s"Content-Length: ${responseBody.length}\r\n\r\n".getBytes(StandardCharsets.UTF_8))
      out.write(responseBody)
      out.flush()
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
      if (current == -1) return null
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
      if (line == null) return ""
      while (line.nonEmpty) {
        val lower = line.toLowerCase
        if (lower.startsWith("content-length:")) {
          contentLength = lower.stripPrefix("content-length:").trim.toInt
        }
        line = readLine(in)
        if (line == null) return ""
      }
      if (contentLength == 0) return ""
      val bytes = new Array[Byte](contentLength)
      var offset = 0
      while (offset < contentLength) {
        val read = in.read(bytes, offset, contentLength - offset)
        if (read == -1) return ""
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

    val routePlans = DapHttpServerMain.buildRoutePlansFromModel(model).routes
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
          Ref.of[IO, RoutePlansLoadResult](RoutePlansLoadResult(routePlans, Nil))
        )
        server <- EmberServerBuilder
          .default[IO]
          .withHost(Host.fromString("127.0.0.1").get)
          .withPort(Port.fromInt(0).get)
          .withHttpApp(
            DapHttpServerMain
              .routes(plansRef, new DapHttpServerMain.SocketDapClient("127.0.0.1", dummyDap.port))
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

  test("POST /resume completes DAP initialize and continue handshake") {
    val dummyDap = new DummyDapServer(Map.empty)
    try {
      val result = new DapHttpServerMain.SocketDapClient("127.0.0.1", dummyDap.port)
        .continueExecution()
        .unsafeRunSync()(cats.effect.unsafe.IORuntime.global)

      assert(result.isRight)
      assert(
        result.toOption.get.hcursor.downField("allThreadsContinued").as[Boolean].contains(true)
      )
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
