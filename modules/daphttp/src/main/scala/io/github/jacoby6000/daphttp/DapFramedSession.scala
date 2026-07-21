package io.github.jacoby6000.daphttp

import cats.effect.IO
import fs2.concurrent.Topic
import io.circe.Json

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.nio.charset.StandardCharsets
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger

final case class WatchHandle(watchId: Int, address: Long, count: Int)

final case class MemoryChangedEvent(
    watchId: Int,
    address: Long,
    count: Int,
    dataBase64: String
)

/** Shared DAP Content-Length framing with a dedicated reader thread that demuxes responses and
  * forwards `dolphin_memoryChanged` events. Writers only enqueue a Deferred/Future and send; they
  * never read the socket (required so idle watches still deliver events).
  */
private[daphttp] final class DapFramedSession(
    in: BufferedInputStream,
    out: BufferedOutputStream,
    publishMemoryChanged: MemoryChangedEvent => Unit,
    closeTransport: () => Unit
) {
  private val seqCounter = new AtomicInteger(1)
  private val initialized = new AtomicBoolean(false)
  private val openFlag = new AtomicBoolean(true)
  private val writeLock = new AnyRef
  private val pending =
    new ConcurrentHashMap[Integer, CompletableFuture[Either[String, Json]]]()

  private val reader = new Thread(
    () => readLoop(),
    "dap-framed-reader"
  )
  reader.setDaemon(true)
  reader.start()

  def isOpen: Boolean = openFlag.get()

  def close(): Unit = {
    if (openFlag.compareAndSet(true, false)) {
      failAllPending("DAP session closed.")
      try closeTransport()
      catch { case _: Exception => () }
      reader.interrupt()
    }
  }

  def sendRequest(
      command: String,
      arguments: Option[Json],
      timeoutMs: Int
  ): Either[String, Json] =
    initialize(timeoutMs).flatMap { _ =>
      val requestSeq = nextSeq()
      val future = new CompletableFuture[Either[String, Json]]()
      pending.put(Integer.valueOf(requestSeq), future)
      try {
        writeRequest(requestSeq, command, arguments)
        Option(future.get(timeoutMs.toLong, TimeUnit.MILLISECONDS)).getOrElse(
          Left(s"Timed out waiting for DAP $command response.")
        )
      } catch {
        case _: java.util.concurrent.TimeoutException =>
          pending.remove(Integer.valueOf(requestSeq))
          Left(s"Timed out waiting for DAP $command response after ${timeoutMs}ms.")
        case error: Exception =>
          pending.remove(Integer.valueOf(requestSeq))
          Left(Option(error.getMessage).getOrElse(error.toString))
      }
    }

  def realtimeWatch(address: Long, count: Int, timeoutMs: Int): Either[String, WatchHandle] =
    sendRequest(
      "dolphin_realtimeWatch",
      Some(
        Json.obj(
          "memoryReference" -> Json.fromString(DapAddress.format(address)),
          "count" -> Json.fromInt(count)
        )
      ),
      timeoutMs
    ).flatMap(parseWatchHandle)

  def realtimeWatchCancel(watchId: Int, timeoutMs: Int): Either[String, Unit] =
    sendRequest(
      "dolphin_realtimeWatchCancel",
      Some(Json.obj("watchId" -> Json.fromInt(watchId))),
      timeoutMs
    ).map(_ => ())

  private def initialize(timeoutMs: Int): Either[String, Unit] =
    if (initialized.get()) {
      Right(())
    } else
      writeLock.synchronized {
        if (initialized.get()) Right(())
        else {
          val requestSeq = nextSeq()
          val future = new CompletableFuture[Either[String, Json]]()
          pending.put(Integer.valueOf(requestSeq), future)
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
          awaitFuture(future, requestSeq, "initialize", timeoutMs).flatMap { body =>
            writeEvent("initialized")
            val needsConfigurationDone = body.hcursor
              .downField("supportsConfigurationDoneRequest")
              .as[Boolean]
              .getOrElse(false)
            // DESNOTE(jbarber, 2026-07-21): Mark the session initialized after the initialize
            // response (and initialized event) so a failed configurationDone cannot restart the
            // full handshake on this connection — adapters typically reject a second initialize.
            // See https://microsoft.github.io/debug-adapter-protocol/overview#initializing
            val configResult =
              if (needsConfigurationDone) {
                val configSeq = nextSeq()
                val configFuture = new CompletableFuture[Either[String, Json]]()
                pending.put(Integer.valueOf(configSeq), configFuture)
                writeRequest(configSeq, "configurationDone", None)
                awaitFuture(configFuture, configSeq, "configurationDone", timeoutMs).map(_ => ())
              } else {
                Right(())
              }
            initialized.set(true)
            configResult
          }
        }
      }

  private def awaitFuture(
      future: CompletableFuture[Either[String, Json]],
      requestSeq: Int,
      command: String,
      timeoutMs: Int
  ): Either[String, Json] =
    try {
      Option(future.get(timeoutMs.toLong, TimeUnit.MILLISECONDS)).getOrElse(
        Left(s"Timed out waiting for DAP $command response.")
      )
    } catch {
      case _: java.util.concurrent.TimeoutException =>
        pending.remove(Integer.valueOf(requestSeq))
        Left(s"Timed out waiting for DAP $command response after ${timeoutMs}ms.")
      case error: Exception =>
        pending.remove(Integer.valueOf(requestSeq))
        Left(Option(error.getMessage).getOrElse(error.toString))
    }

  private def nextSeq(): Int = seqCounter.getAndIncrement()

  private def writeRequest(seq: Int, command: String, arguments: Option[Json]): Unit =
    writeLock.synchronized {
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

  private def writeEvent(event: String): Unit =
    writeLock.synchronized {
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

  private def readLoop(): Unit =
    try {
      while (openFlag.get() && !Thread.currentThread().isInterrupted) {
        try {
          val body = readMessageBody()
          io.circe.parser.parse(body) match {
            case Right(json) =>
              dispatchMessage(json)
            case Left(_) =>
              DapHttpLoggers.dap.warn("Failed to parse DAP message payload.")
          }
        } catch {
          case _: java.net.SocketTimeoutException =>
            () // idle poll; keep draining when data arrives
          case _: InterruptedException =>
            Thread.currentThread().interrupt()
          case error: Exception if openFlag.get() =>
            DapHttpLoggers.dap.warn("DAP reader stopped: {}", error.getMessage)
            openFlag.set(false)
            failAllPending(Option(error.getMessage).getOrElse("DAP reader stopped."))
            try closeTransport()
            catch { case _: Exception => () }
          case _: Exception =>
            ()
        }
      }
    } finally {
      openFlag.set(false)
      failAllPending("DAP session closed.")
    }

  private def dispatchMessage(json: Json): Unit = {
    val cursor = json.hcursor
    cursor.downField("type").as[String].toOption match {
      case Some("response") =>
        cursor.downField("request_seq").as[Int].toOption.foreach { requestSeq =>
          Option(pending.remove(Integer.valueOf(requestSeq))).foreach { future =>
            future.complete(parseDapResponse(json, "request"))
          }
        }
      case Some("event") =>
        cursor.downField("event").as[String].toOption match {
          case Some("dolphin_memoryChanged") =>
            parseMemoryChanged(cursor.downField("body").focus.getOrElse(Json.obj())).foreach {
              event =>
                try publishMemoryChanged(event)
                catch {
                  case error: Exception =>
                    DapHttpLoggers.dap.warn(
                      "memoryChanged publish failed: {}",
                      error.getMessage
                    )
                }
            }
          case Some(other) =>
            DapHttpLoggers.dap.debug("ignoring DAP event {}", other)
          case None =>
            ()
        }
      case other =>
        DapHttpLoggers.dap.debug("ignoring DAP message type {}", other.getOrElse("?"))
    }
  }

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

  private def parseWatchHandle(body: Json): Either[String, WatchHandle] = {
    val cursor = body.hcursor
    for {
      watchId <- cursor.get[Int]("watchId").left.map(_.message)
      count <- cursor.get[Int]("count").left.map(_.message)
      address <- cursor
        .get[String]("address")
        .toOption
        .flatMap(DapAddress.parse)
        .orElse(cursor.get[Long]("address").toOption)
        .toRight("DAP realtimeWatch response missing address.")
    } yield WatchHandle(watchId, address, count)
  }

  private def parseMemoryChanged(body: Json): Option[MemoryChangedEvent] = {
    val cursor = body.hcursor
    for {
      watchId <- cursor.get[Int]("watchId").toOption
      count <- cursor.get[Int]("count").toOption
      data <- cursor.get[String]("data").toOption
      address <- cursor
        .get[String]("address")
        .toOption
        .flatMap(DapAddress.parse)
        .orElse(cursor.get[Long]("address").toOption)
    } yield MemoryChangedEvent(watchId, address, count, data)
  }

  private def failAllPending(message: String): Unit = {
    val iter = pending.entrySet().iterator()
    while (iter.hasNext) {
      val entry = iter.next()
      iter.remove()
      entry.getValue.complete(Left(message))
    }
  }

  private def readMessageBody(): String = {
    val contentLength = readContentLength(in)
    if (contentLength <= 0)
      throw new IllegalStateException("DAP message missing Content-Length.")
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
    if (current == -1 && buffer.isEmpty)
      throw new IllegalStateException("Unexpected EOF while reading DAP headers.")
    buffer.toString()
  }
}

private[daphttp] object DapEventBus {
  def createMemoryChangedTopic(): Topic[IO, MemoryChangedEvent] =
    Topic[IO, MemoryChangedEvent].unsafeRunSync()(cats.effect.unsafe.IORuntime.global)

  def createSessionResetTopic(): Topic[IO, Unit] =
    Topic[IO, Unit].unsafeRunSync()(cats.effect.unsafe.IORuntime.global)

  def publish[A](topic: Topic[IO, A], value: A): Unit =
    topic.publish1(value).unsafeRunAndForget()(cats.effect.unsafe.IORuntime.global)
}
