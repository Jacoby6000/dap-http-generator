package io.github.jacoby6000.daphttp

import cats.effect.IO
import io.circe.Json

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.net.InetSocketAddress
import java.net.Socket
import scala.concurrent.duration._

/** TCP DAP client with monitor-serialized persistent sessions. */
private[daphttp] final class SocketDapClient(
    host: String,
    port: Int,
    dapTimeoutMs: Int = 5000,
    dapContinueTimeoutMs: Int = 30000,
    dapConnectTimeoutMs: Int = 1000,
    dapConnectRetryMs: Int = 5000
) extends DapClient {
  private val connectionLock = new AnyRef
  private var session: Option[DapFramedSession] = None
  private val memoryChangedTopic = DapEventBus.createMemoryChangedTopic()
  private val sessionResetTopic = DapEventBus.createSessionResetTopic()

  private[daphttp] def isConnected: Boolean =
    connectionLock.synchronized(session.exists(_.isOpen))

  override def memoryChanged: fs2.Stream[IO, MemoryChangedEvent] =
    memoryChangedTopic.subscribe(256)

  override def sessionResets: fs2.Stream[IO, Unit] =
    sessionResetTopic.subscribe(32)

  override def startConnectionManager(): IO[Unit] = {
    def maintainConnection: IO[Unit] =
      IO.blocking(isConnected).flatMap {
        case true =>
          IO.sleep(dapConnectRetryMs.millis) *> maintainConnection
        case false =>
          IO.blocking(tryEstablishSession()) flatMap {
            case Right(_) =>
              DapHttpLoggers.dap.info(
                "DAP session ready host={} port={}",
                host,
                Integer.valueOf(port)
              )
              IO.sleep(dapConnectRetryMs.millis) *> maintainConnection
            case Left(error) =>
              DapHttpLoggers.dap.warn(
                "DAP connect failed ({}); retrying in {} ms",
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
        "readMemory host={} port={} address=0x{} bytes={}",
        host,
        Integer.valueOf(port),
        java.lang.Long.toHexString(address),
        Integer.valueOf(sizeBytes)
      )
      withPersistentSession(dapTimeoutMs) { activeSession =>
        FramedDapOps.readMemory(activeSession, address, sizeBytes, dapTimeoutMs)
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
    IO.blocking {
      DapHttpLoggers.dap.debug(
        "writeMemory host={} port={} address=0x{}",
        host,
        Integer.valueOf(port),
        java.lang.Long.toHexString(address)
      )
      withPersistentSession(dapTimeoutMs) { activeSession =>
        FramedDapOps.writeMemory(activeSession, address, dataBase64, dapTimeoutMs)
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
    IO.blocking {
      withPersistentSession(dapTimeoutMs) { activeSession =>
        activeSession.realtimeWatch(address, count, dapTimeoutMs)
      }
    }.handleError(error => Left(error.getMessage))

  override def realtimeWatchCancel(watchId: Int): IO[Either[String, Unit]] =
    IO.blocking {
      withPersistentSession(dapTimeoutMs) { activeSession =>
        activeSession.realtimeWatchCancel(watchId, dapTimeoutMs)
      }
    }.handleError(error => Left(error.getMessage))

  override def continueExecution(threadId: Option[Int] = None): IO[Either[String, Json]] =
    IO.blocking {
      DapHttpLoggers.dap.info("continue host={} port={}", host, Integer.valueOf(port))
      withPersistentSession(dapContinueTimeoutMs) { activeSession =>
        FramedDapOps.continueExecution(
          activeSession,
          threadId,
          dapTimeoutMs,
          dapContinueTimeoutMs
        )
      }
    }.handleError { error =>
      DapHttpLoggers.dap.warn("continue failed: {}", error.getMessage)
      Left(error.getMessage)
    }

  private def withPersistentSession[A](timeoutMs: Int)(
      f: DapFramedSession => A
  ): A = {
    def run(retrying: Boolean): A =
      connectionLock.synchronized {
        val activeSession = ensureSession(timeoutMs)
        try {
          f(activeSession)
        } catch {
          case error: Exception =>
            DapHttpLoggers.dap.warn(
              "DAP connection error (retrying={}): {}",
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

  private def ensureSession(timeoutMs: Int): DapFramedSession =
    connectionLock.synchronized {
      session match {
        case Some(activeSession) if activeSession.isOpen =>
          activeSession
        case stale =>
          if (stale.isDefined) invalidateSessionUnlocked()
          establishSession(timeoutMs, dapConnectTimeoutMs) match {
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
      session match {
        case Some(activeSession) if activeSession.isOpen =>
          Right(())
        case stale =>
          if (stale.isDefined) invalidateSessionUnlocked()
          establishSession(dapTimeoutMs, dapConnectTimeoutMs).map { activeSession =>
            session = Some(activeSession)
          }
      }
    }

  private def establishSession(
      requestTimeoutMs: Int,
      connectTimeoutMs: Int
  ): Either[String, DapFramedSession] = {
    DapHttpLoggers.dap.info(
      "connecting DAP session host={} port={}",
      host,
      Integer.valueOf(port)
    )
    val socket = new Socket()
    try {
      socket.connect(new InetSocketAddress(host, port), connectTimeoutMs)
      // Short SO timeout so the reader thread can wake periodically; request waits use Futures.
      socket.setSoTimeout(math.min(requestTimeoutMs, 1000))
      val activeSession = new DapFramedSession(
        new BufferedInputStream(socket.getInputStream),
        new BufferedOutputStream(socket.getOutputStream),
        event => DapEventBus.publish(memoryChangedTopic, event),
        () =>
          try socket.close()
          catch { case _: Exception => () }
      )
      // First request runs the DAP handshake; threads may fail on some adapters after init.
      activeSession.sendRequest("threads", None, requestTimeoutMs) match {
        case Left(error) if !activeSession.isOpen =>
          activeSession.close()
          Left(error)
        case Left(_) | Right(_) =>
          Right(activeSession)
      }
    } catch {
      case error: Exception =>
        try {
          socket.close()
        } catch {
          case _: Exception => ()
        }
        Left(error.getMessage)
    }
  }

  private def invalidateSession(): Unit =
    connectionLock.synchronized {
      invalidateSessionUnlocked()
    }

  private def invalidateSessionUnlocked(): Unit = {
    session.foreach(_.close())
    session = None
    DapEventBus.publish(sessionResetTopic, ())
  }
}
