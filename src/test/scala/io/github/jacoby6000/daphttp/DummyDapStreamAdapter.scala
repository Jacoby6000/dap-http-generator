package io.github.jacoby6000.daphttp

import io.circe.Json

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.InputStream
import java.io.OutputStream
import java.nio.charset.StandardCharsets
import java.util.Base64

/** Minimal DAP readMemory peer for stream / pipe / Unix-socket tests. */
final class DummyDapStreamAdapter(
    in: InputStream,
    out: OutputStream,
    payloads: Map[(Long, Int), Array[Byte]]
) {
  private val bufferedIn = new BufferedInputStream(in)
  private val bufferedOut = new BufferedOutputStream(out)

  def serveOne(): Unit = {
    val body = readBody(bufferedIn)
    val cursor = io.circe.parser.parse(body).toOption.map(_.hcursor)
    val memoryReference =
      cursor.flatMap(_.downField("arguments").downField("memoryReference").as[String].toOption)
    val count = cursor.flatMap(_.downField("arguments").downField("count").as[Int].toOption)
    val seq = cursor.flatMap(_.downField("seq").as[Int].toOption).getOrElse(1)

    val address = memoryReference.flatMap(parseAddress)
    val data = address.flatMap(addr => count.flatMap(c => payloads.get((addr, c))))
    val responseJson = data match {
      case Some(bytes) =>
        Json.obj(
          "seq" -> Json.fromInt(seq + 1),
          "type" -> Json.fromString("response"),
          "request_seq" -> Json.fromInt(seq),
          "success" -> Json.True,
          "command" -> Json.fromString("readMemory"),
          "body" -> Json.obj(
            "data" -> Json.fromString(Base64.getEncoder.encodeToString(bytes))
          )
        )
      case None =>
        Json.obj(
          "seq" -> Json.fromInt(seq + 1),
          "type" -> Json.fromString("response"),
          "request_seq" -> Json.fromInt(seq),
          "success" -> Json.False,
          "command" -> Json.fromString("readMemory"),
          "message" -> Json.fromString("missing payload")
        )
    }
    val responseBody = responseJson.noSpaces.getBytes(StandardCharsets.UTF_8)
    bufferedOut.write(
      s"Content-Length: ${responseBody.length}\r\n\r\n".getBytes(StandardCharsets.UTF_8)
    )
    bufferedOut.write(responseBody)
    bufferedOut.flush()
  }

  def serveUntilClosed(): Unit = {
    try {
      while (true) serveOne()
    } catch {
      case _: Exception => ()
    }
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
    if (current == -1 && buffer.isEmpty) {
      throw new java.io.EOFException("adapter input closed")
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
