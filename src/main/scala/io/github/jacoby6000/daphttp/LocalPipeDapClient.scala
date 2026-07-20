package io.github.jacoby6000.daphttp

import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.Mutex
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
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration._

/** Client for an existing local DAP endpoint: Windows named pipe or Unix domain socket. */
private[daphttp] final class LocalPipeDapClient(
    path: Path,
    dapTimeoutMs: Int = 5000,
    dapContinueTimeoutMs: Int = 30000,
    dapConnectTimeoutMs: Int = 1000,
    dapConnectRetryMs: Int = 5000
) extends DapHttpServerMain.DapClient {
  private final class OwnedSession(val session: DapStreamSession, val release: IO[Unit])

  private val connectionLock = new AnyRef
  private var ownedSession: Option[OwnedSession] = None

  // DESNOTE(jbarber, 2026-07-19): Serialize all DAP framing I/O on this client. Unlike
  // SocketDapClient (which holds a JVM monitor across the request), pipe sessions are driven by
  // cats-effect IO; Mutex.lock is cancelation-safe and matches TCP's one-in-flight request rule.
  // See https://typelevel.org/cats-effect/docs/std/mutex
  private val sessionMutex: Mutex[IO] =
    Mutex[IO].unsafeRunSync()(cats.effect.unsafe.IORuntime.global)

  private def withSessionLock[A](fa: IO[A]): IO[A] =
    sessionMutex.lock.surround(fa)

  private[daphttp] def isConnected: Boolean =
    connectionLock.synchronized(ownedSession.exists(_.session.isOpen))

  override def startConnectionManager(): IO[Unit] = {
    def maintainConnection: IO[Unit] =
      IO.delay(isConnected).flatMap {
        case true =>
          IO.sleep(dapConnectRetryMs.millis) *> maintainConnection
        case false =>
          withSessionLock(tryEstablishSessionUnlocked).flatMap {
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
    withSessionLock {
      withPersistentSession(dapTimeoutMs) { activeSession =>
        DapHttpLoggers.dap.debug(
          "readMemory pipe={} address=0x{} bytes={}",
          path,
          java.lang.Long.toHexString(address),
          Integer.valueOf(sizeBytes)
        )
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
    withSessionLock {
      ensureSession.flatMap { activeSession =>
        val threadIdIO =
          IO.interruptible(activeSession.sendRequest(command = "threads", arguments = None))
            .timeout(math.min(dapTimeoutMs, 2000).millis)
            .map(_.toOption.flatMap(json => parseThreadIds(json).headOption).getOrElse(1))
            .handleError { _ =>
              DapHttpLoggers.dap.debug("threads unavailable; continuing with threadId=1")
              1
            }

        threadIdIO.flatMap { threadId =>
          DapHttpLoggers.dap.info("continue pipe={}", path)
          IO.interruptible {
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
          }.timeout(dapContinueTimeoutMs.millis)
        }
      }
    }.handleErrorWith { error =>
      DapHttpLoggers.dap.warn("continue failed: {}", error.getMessage)
      withSessionLock(invalidateSession).as(Left(error.getMessage))
    }

  private def withPersistentSession[A](timeoutMs: Int)(f: DapStreamSession => A): IO[A] = {
    def run(retrying: Boolean): IO[A] =
      ensureSession.flatMap { activeSession =>
        IO.interruptible(f(activeSession))
          .timeout(timeoutMs.millis)
          .handleErrorWith { error =>
            DapHttpLoggers.dap.warn(
              "DAP pipe connection error (retrying={}): {}",
              java.lang.Boolean.valueOf(!retrying),
              error.getMessage
            )
            invalidateSession *> {
              if (!retrying) run(retrying = true)
              else IO.raiseError(error)
            }
          }
      }

    run(retrying = false)
  }

  private def ensureSession: IO[DapStreamSession] =
    IO.delay {
      connectionLock.synchronized(ownedSession.filter(_.session.isOpen))
    }.flatMap {
      case Some(owned) => IO.pure(owned.session)
      case None        =>
        establishSession.flatMap {
          case Right(owned) =>
            IO.delay {
              connectionLock.synchronized {
                ownedSession = Some(owned)
              }
              owned.session
            }
          case Left(error) =>
            IO.raiseError(new java.io.IOException(error))
        }
    }

  private def tryEstablishSessionUnlocked: IO[Either[String, Unit]] =
    IO.delay {
      connectionLock.synchronized(ownedSession.exists(_.session.isOpen))
    }.flatMap {
      case true  => IO.pure(Right(()))
      case false =>
        establishSession.flatMap {
          case Right(owned) =>
            IO.delay {
              connectionLock.synchronized {
                ownedSession = Some(owned)
              }
              Right(())
            }
          case Left(error) => IO.pure(Left(error))
        }
    }

  // DESNOTE(jbarber, 2026-07-19): Open + initialize under Resource so a failed handshake always
  // closes the pipe/socket. On success, Resource.allocated transfers ownership to OwnedSession;
  // invalidateSession runs the finalizer later.
  private def establishSession: IO[Either[String, OwnedSession]] = {
    DapHttpLoggers.dap.info("connecting DAP session pipe={}", path)
    openInitializedSession.attempt.map {
      case Right((activeSession, release)) =>
        Right(new OwnedSession(activeSession, release.handleErrorWith(_ => IO.unit)))
      case Left(error) =>
        Left(Option(error.getMessage).getOrElse(error.toString))
    }
  }

  private def openInitializedSession: IO[(DapStreamSession, IO[Unit])] =
    Resource
      .make(openStreamsIO)(opened => IO.blocking(opened.close()))
      .evalMap { opened =>
        IO.interruptible {
          val activeSession = new DapStreamSession(opened.in, opened.out)
          activeSession.initialize() match {
            case Right(_)  => activeSession
            case Left(err) => throw new IllegalStateException(err)
          }
        }.timeout(dapTimeoutMs.millis)
      }
      .allocated

  private def openStreamsIO: IO[OpenedStreams] =
    IO.interruptible(openStreams(path)).timeout(dapConnectTimeoutMs.millis)

  private def invalidateSession: IO[Unit] =
    IO.delay {
      connectionLock.synchronized {
        val current = ownedSession
        ownedSession = None
        current
      }
    }.flatMap {
      case Some(owned) =>
        owned.session.markClosed() *> owned.release
      case None =>
        IO.unit
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
      val closeables: List[Closeable]
  ) {
    def close(): Unit =
      closeables.foreach { c =>
        try c.close()
        catch { case _: Exception => () }
      }
  }

  private def openStreams(pipePath: Path): OpenedStreams =
    if (isWindowsNamedPipePath(pipePath)) {
      // DESNOTE(jbarber, 2026-07-19): Connect as a client to an *existing* Windows named pipe.
      // See https://learn.microsoft.com/en-us/windows/win32/ipc/pipe-names
      val raf = new RandomAccessFile(pipePath.toString, "rw")
      val channel = raf.getChannel
      new OpenedStreams(
        Channels.newInputStream(channel),
        Channels.newOutputStream(channel),
        List(raf)
      )
    } else {
      // DESNOTE(jbarber, 2026-07-19): dolphin-dap DAPSocket is an AF_UNIX path, not a FIFO.
      // See https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/nio/channels/SocketChannel.html
      val channel = SocketChannel.open(StandardProtocolFamily.UNIX)
      try {
        channel.connect(UnixDomainSocketAddress.of(pipePath))
        new OpenedStreams(
          Channels.newInputStream(channel),
          Channels.newOutputStream(channel),
          List(channel)
        )
      } catch {
        case error: Exception =>
          try channel.close()
          catch { case _: Exception => () }
          throw error
      }
    }

  private def isWindowsNamedPipePath(pipePath: Path): Boolean = {
    val normalized = pipePath.toString.replace('/', '\\').toLowerCase
    normalized.startsWith("""\\.\pipe\""") || normalized.startsWith("""\\?\pipe\""")
  }

  /** Persistent DAP framing session over arbitrary byte streams. */
  private final class DapStreamSession(rawIn: InputStream, rawOut: OutputStream) {
    private val in = new BufferedInputStream(rawIn)
    private val out = new BufferedOutputStream(rawOut)
    private var seqCounter = 1
    private var initialized = false
    private val openFlag = new AtomicBoolean(true)

    def isOpen: Boolean = openFlag.get()

    def markClosed(): IO[Unit] =
      IO.delay { val _ = openFlag.set(false) }

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
          dapConnectTimeoutMs,
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
