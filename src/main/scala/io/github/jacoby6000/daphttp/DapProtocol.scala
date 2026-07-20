package io.github.jacoby6000.daphttp

import io.circe.Json

import java.io.BufferedInputStream
import java.io.InputStream
import java.io.OutputStream
import java.nio.charset.StandardCharsets
import java.util.Base64

/** DAP Content-Length framing and readMemory request/response helpers. */
private[daphttp] object DapProtocol {
  def buildReadMemoryRequest(seq: Int, address: Long, sizeBytes: Int): Array[Byte] = {
    val request =
      Json
        .obj(
          "seq" -> Json.fromInt(seq),
          "type" -> Json.fromString("request"),
          "command" -> Json.fromString("readMemory"),
          "arguments" -> Json.obj(
            "memoryReference" -> Json.fromString(f"0x$address%x"),
            "count" -> Json.fromInt(sizeBytes)
          )
        )
        .noSpaces
    request.getBytes(StandardCharsets.UTF_8)
  }

  def writeFramed(out: OutputStream, body: Array[Byte]): Unit = {
    out.write(s"Content-Length: ${body.length}\r\n\r\n".getBytes(StandardCharsets.UTF_8))
    out.write(body)
    out.flush()
  }

  def readFramedMessage(in: InputStream): String = {
    val buffered = in match {
      case already: BufferedInputStream => already
      case other                        => new BufferedInputStream(other)
    }
    val contentLength = readContentLength(buffered)
    readBody(buffered, contentLength)
  }

  def parseReadMemoryResponse(body: String): Either[String, String] =
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
        Left(
          json.hcursor
            .downField("message")
            .as[String]
            .toOption
            .getOrElse("DAP readMemory failed")
        )
      case None =>
        Left("Failed to parse DAP response payload.")
    }

  /** Returns None when the message is not the response for `expectedSeq` (e.g. an event). */
  def parseReadMemoryResponseForSeq(
      body: String,
      expectedSeq: Int
  ): Option[Either[String, String]] = {
    io.circe.parser.parse(body).toOption.flatMap { json =>
      val cursor = json.hcursor
      val messageType = cursor.downField("type").as[String].toOption
      val requestSeq = cursor.downField("request_seq").as[Int].toOption
      messageType match {
        case Some("response") if requestSeq.contains(expectedSeq) =>
          Some(parseReadMemoryResponse(body))
        case Some("response") =>
          None
        case _ =>
          None
      }
    }
  }

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
      if (bytesRead == -1)
        throw new IllegalStateException("Unexpected EOF while reading DAP response body.")
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
