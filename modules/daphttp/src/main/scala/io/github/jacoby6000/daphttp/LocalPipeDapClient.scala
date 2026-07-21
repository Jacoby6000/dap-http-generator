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
  private final class OwnedSession(val session: DapFramedSession, val release: IO[Unit])

  private val connectionLock = new AnyRef
  private var ownedSession: Option[OwnedSession] = None
  private val memoryChangedTopic = DapEventBus.createMemoryChangedTopic()
  private val sessionResetTopic = DapEventBus.createSessionResetTopic()

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

  override def memoryChanged: fs2.Stream[IO, MemoryChangedEvent] =
    memoryChangedTopic.subscribe(256)

  override def sessionResets: fs2.Stream[IO, Unit] =
    sessionResetTopic.subscribe(32)

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
            ),
            timeoutMs = dapTimeoutMs
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

  override def writeMemory(address: Long, dataBase64: String): IO[Either[String, Int]] =
    withSessionLock {
      withPersistentSession(dapTimeoutMs) { activeSession =>
        DapHttpLoggers.dap.debug(
          "writeMemory pipe={} address=0x{}",
          path,
          java.lang.Long.toHexString(address)
        )
        activeSession
          .sendRequest(
            command = "writeMemory",
            arguments = Some(
              Json.obj(
                "memoryReference" -> Json.fromString(f"0x$address%x"),
                "data" -> Json.fromString(dataBase64)
              )
            ),
            timeoutMs = dapTimeoutMs
          )
          .flatMap { body =>
            body.hcursor
              .get[Int]("bytesWritten")
              .toOption
              .toRight("DAP writeMemory response did not include body.bytesWritten.")
          } match {
          case Right(written) =>
            DapHttpLoggers.dap.debug(
              "writeMemory address=0x{} succeeded bytesWritten={}",
              java.lang.Long.toHexString(address),
              Integer.valueOf(written)
            )
            Right(written)
          case Left(error) =>
            DapHttpLoggers.dap.warn(
              "writeMemory address=0x{} failed: {}",
              java.lang.Long.toHexString(address),
              error
            )
            Left(error)
        }
      }
    }.handleError { error =>
      DapHttpLoggers.dap.warn(
        "writeMemory address=0x{} failed: {}",
        java.lang.Long.toHexString(address),
        error.getMessage
      )
      Left(error.getMessage)
    }

  override def realtimeWatch(address: Long, count: Int): IO[Either[String, WatchHandle]] =
    withSessionLock {
      withPersistentSession(dapTimeoutMs) { activeSession =>
        activeSession.realtimeWatch(address, count, dapTimeoutMs)
      }
    }.handleError(error => Left(error.getMessage))

  override def realtimeWatchCancel(watchId: Int): IO[Either[String, Unit]] =
    withSessionLock {
      withPersistentSession(dapTimeoutMs) { activeSession =>
        activeSession.realtimeWatchCancel(watchId, dapTimeoutMs)
      }
    }.handleError(error => Left(error.getMessage))

  override def continueExecution(threadId: Option[Int] = None): IO[Either[String, Json]] =
    withSessionLock {
      ensureSession.flatMap { activeSession =>
        val threadIdIO = threadId match {
          case Some(id) => IO.pure(id)
          case None     =>
            IO.interruptible(
              activeSession.sendRequest(
                command = "threads",
                arguments = None,
                timeoutMs = math.min(dapTimeoutMs, 2000)
              )
            ).timeout(math.min(dapTimeoutMs, 2000).millis)
              .map(_.toOption.flatMap(json => parseThreadIds(json).headOption).getOrElse(1))
              .handleError { _ =>
                DapHttpLoggers.dap.debug("threads unavailable; continuing with threadId=1")
                1
              }
        }

        threadIdIO.flatMap { resolvedThreadId =>
          DapHttpLoggers.dap.info("continue pipe={}", path)
          IO.interruptible {
            activeSession
              .sendRequest(
                command = "continue",
                arguments = Some(Json.obj("threadId" -> Json.fromInt(resolvedThreadId))),
                timeoutMs = dapContinueTimeoutMs
              )
              .map { response =>
                DapHttpLoggers.dap.info(
                  "continue threadId={} succeeded",
                  Integer.valueOf(resolvedThreadId)
                )
                response
              }
              .left
              .map { error =>
                DapHttpLoggers.dap.warn(
                  "continue threadId={} failed: {}",
                  Integer.valueOf(resolvedThreadId),
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

  private def withPersistentSession[A](timeoutMs: Int)(f: DapFramedSession => A): IO[A] = {
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

  private def ensureSession: IO[DapFramedSession] =
    IO.delay {
      connectionLock.synchronized(ownedSession)
    }.flatMap {
      case Some(owned) if owned.session.isOpen =>
        IO.pure(owned.session)
      case Some(_) =>
        invalidateSession *> establishSession.flatMap {
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
      case None =>
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
      connectionLock.synchronized(ownedSession)
    }.flatMap {
      case Some(owned) if owned.session.isOpen =>
        IO.pure(Right(()))
      case Some(_) =>
        invalidateSession *> establishSession.flatMap {
          case Right(owned) =>
            IO.delay {
              connectionLock.synchronized {
                ownedSession = Some(owned)
              }
              Right(())
            }
          case Left(error) => IO.pure(Left(error))
        }
      case None =>
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

  private def openInitializedSession: IO[(DapFramedSession, IO[Unit])] =
    Resource
      .make(openStreamsIO)(opened => IO.blocking(opened.close()))
      .evalMap { opened =>
        IO.interruptible {
          val activeSession = new DapFramedSession(
            new BufferedInputStream(opened.in),
            new BufferedOutputStream(opened.out),
            event => DapEventBus.publish(memoryChangedTopic, event),
            () => opened.close()
          )
          activeSession.sendRequest("threads", None, dapTimeoutMs) match {
            case Left(error) if !activeSession.isOpen =>
              activeSession.close()
              throw new IllegalStateException(error)
            case Left(_) | Right(_) =>
              activeSession
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
        IO.blocking(owned.session.close()) *> owned.release <* IO.delay(
          DapEventBus.publish(sessionResetTopic, ())
        )
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
    private val closed = new AtomicBoolean(false)
    def close(): Unit =
      if (closed.compareAndSet(false, true)) {
        closeables.foreach { c =>
          try c.close()
          catch { case _: Exception => () }
        }
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
