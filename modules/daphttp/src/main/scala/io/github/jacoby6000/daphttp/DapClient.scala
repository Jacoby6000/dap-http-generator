package io.github.jacoby6000.daphttp

import cats.effect.IO
import io.circe.Json

/** DAP adapter client used by HTTP routes and realtime watches. */
private[daphttp] trait DapClient {
  def readMemory(address: Long, sizeBytes: Int): IO[Either[String, String]]
  def writeMemory(address: Long, dataBase64: String): IO[Either[String, Int]] = {
    val _ = (address, dataBase64)
    IO.pure(Left("writeMemory is not supported by this DAP client."))
  }
  def continueExecution(threadId: Option[Int] = None): IO[Either[String, Json]]
  def startConnectionManager(): IO[Unit] = IO.unit
  def realtimeWatch(address: Long, count: Int): IO[Either[String, WatchHandle]] = {
    val _ = (address, count)
    IO.pure(Left("Realtime watch is not supported by this DAP client."))
  }
  def realtimeWatchCancel(watchId: Int): IO[Either[String, Unit]] = {
    val _ = watchId
    IO.pure(Left("Realtime watch cancel is not supported by this DAP client."))
  }
  def memoryChanged: fs2.Stream[IO, MemoryChangedEvent] = fs2.Stream.empty
  def sessionResets: fs2.Stream[IO, Unit] = fs2.Stream.empty
}
