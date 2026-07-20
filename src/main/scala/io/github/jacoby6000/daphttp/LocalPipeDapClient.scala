package io.github.jacoby6000.daphttp

import cats.effect.IO
import io.circe.Json

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.Closeable
import java.io.InputStream
import java.io.OutputStream
import java.io.RandomAccessFile
import java.net.StandardProtocolFamily
import java.net.UnixDomainSocketAddress
import java.nio.channels.Channels
import java.nio.channels.SocketChannel
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import scala.concurrent.duration._

/** Client for an existing local DAP endpoint: Windows named pipe or Unix domain socket. */
private[daphttp] final class LocalPipeDapClient(
    path: Path,
    dapTimeoutMs: Int = 5000,
    dapContinueTimeoutMs: Int = 30000,
    dapConnectRetryMs: Int = 5000
) extends DapHttpServerMain.DapClient {
  private val connectionLock = new AnyRef
  private var session: Option[DapStreamSession] = None
  private var closeables: List[Closeable] = Nil

  private[daphttp] def isConnected: Boolean =
    connectionLock.synchronized(session.exists(_.isOpen))

  override def startConnectionManager(): IO[Unit] = {
    def maintainConnection: IO[Unit] =
      IO.blocking(isConnected).flatMap {
        case true =>
          IO.sleep(dapConnectRetryMs.millis) *> maintainConnection
        case false =>
          IO.blocking(tryEstablishSession()) flatMap {
            case Right(_) =>
              DapHttpLoggers.dap.info("DAP session ready pipe={}", path)
              IO.sleep(dapConnectRetryMs.millis) *> maintainConnection
            case Left(error) =>
              DapHttpLoggers.dap.warn(
                "DAP pipe connect failed ({}); retrying in {} ms",
                error,
                Integer.valueOf(dapConnectRetryMs)
              )
              IO.sleep(dapConnectRetryMs.millis) *> maintainConnection
          }
      }

    maintainConnection.start.void
  }

  override def readMemory(address: Long, sizeBytes: Int): IO[Either[String, String]] =
    IO.blocking {
      DapHttpLoggers.dap.debug(
        "readMemory pipe={} address=0x{} bytes={}",
        path,
        java.lang.Long.toHexString(address),
        Integer.valueOf(sizeBytes)
      )

      withPersistentSession { activeSession =>
        activeSession
          .sendRequest(
            command = "readMemory",
            arguments = Some(
              Json.obj(
                "memoryReference" -> Json.fromString(f"0x$address%x"),
                "count" -> Json.fromInt(sizeBytes)
              )
            )
          )
          .flatMap { body =>
            body.hcursor
              .downField("data")
              .as[String]
              .toOption
              .toRight("DAP readMemory response did not include body.data.")
          } match {
          case Right(value) =>
            DapHttpLoggers.dap.debug(
              "readMemory address=0x{} succeeded bytes={}",
              java.lang.Long.toHexString(address),
              Integer.valueOf(sizeBytes)
            )
            Right(value)
          case Left(error) =>
            DapHttpLoggers.dap.warn(
              "readMemory address=0x{} failed: {}",
              java.lang.Long.toHexString(address),
              error
            )
            Left(error)
        }
      }
    }.handleError { error =>
      DapHttpLoggers.dap.warn(
        "readMemory address=0x{} failed: {}",
        java.lang.Long.toHexString(address),
        error.getMessage
      )
      Left(error.getMessage)
    }

  override def continueExecution(): IO[Either[String, Json]] =
    IO.blocking {
      DapHttpLoggers.dap.info("continue pipe={}", path)
      val _ = dapContinueTimeoutMs
      withPersistentSession { activeSession =>
        val threadId =
          activeSession
            .trySendRequest(
              command = "threads",
              arguments = None,
              requestTimeoutMs = math.min(dapTimeoutMs, 2000)
            )
            .flatMap(json => parseThreadIds(json).headOption)
            .getOrElse {
              DapHttpLoggers.dap.debug("threads unavailable; continuing with threadId=1")
              1
            }

        activeSession
          .sendRequest(
            command = "continue",
            arguments = Some(Json.obj("threadId" -> Json.fromInt(threadId)))
          )
          .map { response =>
            DapHttpLoggers.dap.info("continue threadId={} succeeded", Integer.valueOf(threadId))
            response
          }
          .left
          .map { error =>
            DapHttpLoggers.dap.warn(
              "continue threadId={} failed: {}",
              Integer.valueOf(threadId),
              error
            )
            error
          }
      }
    }.handleError { error =>
      DapHttpLoggers.dap.warn("continue failed: {}", error.getMessage)
      Left(error.getMessage)
    }

  private def withPersistentSession[A](f: DapStreamSession => A): A = {
    def run(retrying: Boolean): A =
      connectionLock.synchronized {
        val activeSession = ensureSession()
        try {
          f(activeSession)
        } catch {
          case error: Exception =>
            DapHttpLoggers.dap.warn(
              "DAP pipe connection error (retrying={}): {}",
              java.lang.Boolean.valueOf(!retrying),
              error.getMessage
            )
            invalidateSession()
            if (!retrying) run(retrying = true)
            else throw error
        }
      }

    run(retrying = false)
  }

  private def ensureSession(): DapStreamSession =
    connectionLock.synchronized {
      session.filter(_.isOpen) match {
        case Some(activeSession) =>
          activeSession
        case None =>
          establishSession() match {
            case Right(activeSession) =>
              session = Some(activeSession)
              activeSession
            case Left(error) =>
              throw new java.io.IOException(error)
          }
      }
    }

  private def tryEstablishSession(): Either[String, Unit] =
    connectionLock.synchronized {
      session.filter(_.isOpen) match {
        case Some(_) => Right(())
        case None    =>
          establishSession().map { activeSession =>
            session = Some(activeSession)
          }
      }
    }

  private def establishSession(): Either[String, DapStreamSession] = {
    DapHttpLoggers.dap.info("connecting DAP session pipe={}", path)
    try {
      val opened = openStreams(path)
      closeables = opened.closeables
      val activeSession = new DapStreamSession(opened.in, opened.out, () => opened.close())
      activeSession.initialize().map(_ => activeSession)
    } catch {
      case error: Exception =>
        invalidateSession()
        Left(error.getMessage)
    }
  }

  private def invalidateSession(): Unit =
    connectionLock.synchronized {
      session.foreach(_.close())
      session = None
      closeables.foreach { c =>
        try c.close()
        catch { case _: Exception => () }
      }
      closeables = Nil
    }

  private def parseThreadIds(responseBody: Json): List[Int] =
    responseBody.hcursor
      .downField("threads")
      .values
      .getOrElse(Vector.empty)
      .flatMap(_.hcursor.downField("id").as[Int].toOption)
      .toList

  private final class OpenedStreams(
      val in: InputStream,
      val out: OutputStream,
      val closeables: List[Closeable],
      val close: () => Unit
  )

  private def openStreams(pipePath: Path): OpenedStreams =
    if (isWindowsNamedPipePath(pipePath)) {
      // DESNOTE(jbarber, 2026-07-19): Connect as a client to an *existing* Windows named pipe.
      // See https://learn.microsoft.com/en-us/windows/win32/ipc/pipe-names
      val raf = new RandomAccessFile(pipePath.toString, "rw")
      val channel = raf.getChannel
      new OpenedStreams(
        Channels.newInputStream(channel),
        Channels.newOutputStream(channel),
        List(raf),
        () => raf.close()
      )
    } else {
      // DESNOTE(jbarber, 2026-07-19): dolphin-dap DAPSocket is an AF_UNIX path, not a FIFO.
      // See https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/nio/channels/SocketChannel.html
      val channel = SocketChannel.open(StandardProtocolFamily.UNIX)
      channel.connect(UnixDomainSocketAddress.of(pipePath))
      new OpenedStreams(
        Channels.newInputStream(channel),
        Channels.newOutputStream(channel),
        List(channel),
        () => channel.close()
      )
    }

  private def isWindowsNamedPipePath(pipePath: Path): Boolean = {
    val normalized = pipePath.toString.replace('/', '\\').toLowerCase
    normalized.startsWith("""\\.\pipe\""") || normalized.startsWith("""\\?\pipe\""")
  }

  /** Persistent DAP framing session over arbitrary byte streams. */
  private final class DapStreamSession(
      rawIn: InputStream,
      rawOut: OutputStream,
      closeFn: () => Unit
  ) {
    private val in = new BufferedInputStream(rawIn)
    private val out = new BufferedOutputStream(rawOut)
    private var seqCounter = 1
    private var initialized = false
    private var open = true

    def isOpen: Boolean = open

    def close(): Unit = {
      open = false
      try closeFn()
      catch { case _: Exception => () }
    }

    def sendRequest(command: String, arguments: Option[Json]): Either[String, Json] =
      initialize().flatMap { _ =>
        val requestSeq = nextSeq()
        writeRequest(requestSeq, command, arguments)
        readUntilResponse(requestSeq, command)
      }

    def initialize(): Either[String, Unit] =
      if (initialized) {
        Right(())
      } else {
        val requestSeq = nextSeq()
        writeRequest(
          requestSeq,
          "initialize",
          Some(
            Json.obj(
              "clientID" -> Json.fromString("dap-http-generator"),
              "clientName" -> Json.fromString("dap-http-generator"),
              "adapterID" -> Json.fromString("dap-http-generator"),
              "pathFormat" -> Json.fromString("path"),
              "linesStartAt1" -> Json.True,
              "columnsStartAt1" -> Json.True,
              "supportsVariableType" -> Json.True,
              "supportsVariablePaging" -> Json.False,
              "supportsRunInTerminalRequest" -> Json.False
            )
          )
        )
        readUntilResponse(requestSeq, "initialize").flatMap { body =>
          writeEvent("initialized")
          val needsConfigurationDone = body.hcursor
            .downField("supportsConfigurationDoneRequest")
            .as[Boolean]
            .getOrElse(false)
          if (needsConfigurationDone) {
            val configSeq = nextSeq()
            writeRequest(configSeq, "configurationDone", None)
            readUntilResponse(configSeq, "configurationDone").map { _ =>
              initialized = true
              ()
            }
          } else {
            initialized = true
            Right(())
          }
        }
      }

    def trySendRequest(
        command: String,
        arguments: Option[Json],
        requestTimeoutMs: Int
    ): Option[Json] = {
      // DESNOTE(jbarber, 2026-07-19): Unix domain sockets / named pipes do not expose the same
      // per-call SO_TIMEOUT control as TCP Socket; optional probes may block until the peer
      // replies. Keep the API for parity with SocketDapClient.
      val _ = requestTimeoutMs
      try {
        sendRequest(command, arguments).toOption
      } catch {
        case _: Exception => None
      }
    }

    private def nextSeq(): Int = {
      val value = seqCounter
      seqCounter += 1
      value
    }

    private def writeRequest(seq: Int, command: String, arguments: Option[Json]): Unit = {
      val request = arguments match {
        case Some(args) =>
          Json.obj(
            "seq" -> Json.fromInt(seq),
            "type" -> Json.fromString("request"),
            "command" -> Json.fromString(command),
            "arguments" -> args
          )
        case None =>
          Json.obj(
            "seq" -> Json.fromInt(seq),
            "type" -> Json.fromString("request"),
            "command" -> Json.fromString(command)
          )
      }
      writeMessage(request)
    }

    private def writeEvent(event: String): Unit = {
      val payload = Json.obj(
        "seq" -> Json.fromInt(nextSeq()),
        "type" -> Json.fromString("event"),
        "event" -> Json.fromString(event)
      )
      writeMessage(payload)
    }

    private def writeMessage(json: Json): Unit = {
      val payload = json.noSpaces.getBytes(StandardCharsets.UTF_8)
      out.write(s"Content-Length: ${payload.length}\r\n\r\n".getBytes(StandardCharsets.UTF_8))
      out.write(payload)
      out.flush()
    }

    private def readUntilResponse(requestSeq: Int, command: String): Either[String, Json] = {
      var skippedEvents = 0
      while (skippedEvents < 64) {
        val body = readMessageBody()
        io.circe.parser.parse(body).toOption match {
          case Some(json) if isMatchingResponse(json, requestSeq) =>
            return parseDapResponse(json, command)
          case Some(json) if json.hcursor.downField("type").as[String].contains("event") =>
            DapHttpLoggers.dap.debug(
              "skipping DAP event {} while waiting for {} response",
              json.hcursor.downField("event").as[String].getOrElse("?"),
              command
            )
            skippedEvents += 1
          case Some(json) =>
            DapHttpLoggers.dap.debug(
              "skipping unexpected DAP message while waiting for {} response: {}",
              command,
              json.noSpaces
            )
            skippedEvents += 1
          case None =>
            return Left(s"Failed to parse DAP $command response payload.")
        }
      }
      Left(s"Timed out waiting for DAP $command response.")
    }

    private def isMatchingResponse(json: Json, requestSeq: Int): Boolean =
      json.hcursor.downField("type").as[String].contains("response") &&
        json.hcursor.downField("request_seq").as[Int].contains(requestSeq)

    private def parseDapResponse(json: Json, command: String): Either[String, Json] =
      if (json.hcursor.downField("success").as[Boolean].getOrElse(false)) {
        Right(json.hcursor.downField("body").focus.getOrElse(Json.Null))
      } else {
        val message = json.hcursor
          .downField("message")
          .as[String]
          .toOption
          .getOrElse(s"DAP $command failed")
        Left(message)
      }

    private def readMessageBody(): String = {
      val contentLength = readContentLength(in)
      readBody(in, contentLength)
    }

    private def readContentLength(input: BufferedInputStream): Int = {
      var contentLength = 0
      var line = readLine(input)
      while (line.nonEmpty) {
        val lower = line.toLowerCase
        if (lower.startsWith("content-length:")) {
          contentLength = lower.stripPrefix("content-length:").trim.toInt
        }
        line = readLine(input)
      }
      contentLength
    }

    private def readBody(input: BufferedInputStream, length: Int): String = {
      val buffer = new Array[Byte](length)
      var read = 0
      while (read < length) {
        val bytesRead = input.read(buffer, read, length - read)
        if (bytesRead == -1)
          throw new IllegalStateException("Unexpected EOF while reading DAP response body.")
        read += bytesRead
      }
      new String(buffer, StandardCharsets.UTF_8)
    }

    private def readLine(input: BufferedInputStream): String = {
      val buffer = new StringBuilder
      var current = input.read()
      var previous = -1
      while (current != -1 && !(previous == '\r' && current == '\n')) {
        if (current != '\r') buffer.append(current.toChar)
        previous = current
        current = input.read()
      }
      buffer.toString()
    }
  }
}

private[daphttp] object DapClients {
  def create(
      dapPipe: Option[Path],
      dapHost: String,
      dapPort: Int,
      dapTimeoutMs: Int,
      dapContinueTimeoutMs: Int,
      dapConnectTimeoutMs: Int,
      dapConnectRetryMs: Int
  ): DapHttpServerMain.DapClient =
    dapPipe match {
      case Some(path) =>
        new LocalPipeDapClient(
          path,
          dapTimeoutMs,
          dapContinueTimeoutMs,
          dapConnectRetryMs
        )
      case None =>
        new DapHttpServerMain.SocketDapClient(
          dapHost,
          dapPort,
          dapTimeoutMs,
          dapContinueTimeoutMs,
          dapConnectTimeoutMs,
          dapConnectRetryMs
        )
    }
}
