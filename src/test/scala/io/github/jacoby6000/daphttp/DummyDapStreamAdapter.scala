package io.github.jacoby6000.daphttp

import io.circe.Json

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.InputStream
import java.io.OutputStream
import java.nio.charset.StandardCharsets
import java.util.Base64

/** Minimal DAP peer for stream / Unix-socket tests (initialize + readMemory). */
final class DummyDapStreamAdapter(
    in: InputStream,
    out: OutputStream,
    payloads: Map[(Long, Int), Array[Byte]]
) {
  private val bufferedIn = new BufferedInputStream(in)
  private val bufferedOut = new BufferedOutputStream(out)

  def serveUntilClosed(): Unit = {
    try {
      var keepReading = true
      while (keepReading) {
        val body = readBody(bufferedIn)
        if (body == null) {
          keepReading = false
        } else {
          val cursor = io.circe.parser.parse(body).toOption.map(_.hcursor)
          val messageType = cursor.flatMap(_.downField("type").as[String].toOption)
          val command = cursor.flatMap(_.downField("command").as[String].toOption)
          val requestSeq = cursor.flatMap(_.downField("seq").as[Int].toOption).getOrElse(1)

          (messageType, command) match {
            case (Some("request"), Some("initialize")) =>
              writeResponse(
                bufferedOut,
                requestSeq,
                "initialize",
                Json.obj("supportsConfigurationDoneRequest" -> Json.False)
              )
            case (Some("event"), _) =>
              ()
            case (Some("request"), Some("configurationDone")) =>
              writeResponse(bufferedOut, requestSeq, "configurationDone", Json.obj())
            case (Some("request"), Some("readMemory")) =>
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
                    bufferedOut,
                    requestSeq,
                    "readMemory",
                    Json.obj("data" -> Json.fromString(Base64.getEncoder.encodeToString(bytes)))
                  )
                  keepReading = false
                case None =>
                  writeFailure(bufferedOut, requestSeq, "readMemory", "missing payload")
                  keepReading = false
              }
            case (Some("request"), Some(other)) =>
              writeFailure(bufferedOut, requestSeq, other, s"unsupported command $other")
            case _ =>
              keepReading = false
          }
        }
      }
    } catch {
      case _: Exception => ()
    }
  }

  def serveOne(): Unit = serveUntilClosed()

  private def parseAddress(memoryReference: String): Option[Long] = {
    val trimmed = memoryReference.trim
    if (trimmed.startsWith("0x") || trimmed.startsWith("0X")) {
      scala.util.Try(java.lang.Long.parseUnsignedLong(trimmed.drop(2), 16)).toOption
    } else {
      scala.util.Try(trimmed.toLong).toOption
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
    if (line == null) return null
    while (line != null && line.nonEmpty) {
      val lower = line.toLowerCase
      if (lower.startsWith("content-length:")) {
        contentLength = lower.stripPrefix("content-length:").trim.toInt
      }
      line = readLine(in)
      if (line == null) return null
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
