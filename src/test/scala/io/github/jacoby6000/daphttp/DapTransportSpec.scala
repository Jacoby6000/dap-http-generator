package io.github.jacoby6000.daphttp

import cats.syntax.all._
import org.scalatest.funsuite.AnyFunSuite

import java.net.StandardProtocolFamily
import java.net.UnixDomainSocketAddress
import java.nio.channels.Channels
import java.nio.channels.ServerSocketChannel
import java.nio.file.Files
import java.util.Base64
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit

class DapTransportSpec extends AnyFunSuite {
  private val payloads: Map[(Long, Int), Array[Byte]] =
    Map(
      (0x1000L, 2) -> Array(0x34.toByte, 0x12.toByte),
      (0x2000L, 2) -> Array(0x78.toByte, 0x56.toByte)
    )

  test("LocalPipeDapClient speaks DAP over a Unix domain socket path") {
    val dir = Files.createTempDirectory("dap-unix")
    val sockPath = dir.resolve("dap.sock")
    val server = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
    server.bind(UnixDomainSocketAddress.of(sockPath))

    val started = new CountDownLatch(1)
    val adapterThread = new Thread(
      () => {
        started.countDown()
        val peer = server.accept()
        try {
          new DummyDapStreamAdapter(
            Channels.newInputStream(peer),
            Channels.newOutputStream(peer),
            payloads
          ).serveUntilClosed()
        } finally {
          peer.close()
        }
      },
      "dummy-unix-adapter"
    )
    adapterThread.setDaemon(true)
    adapterThread.start()

    assert(started.await(5, TimeUnit.SECONDS))
    val client = new LocalPipeDapClient(sockPath, dapConnectRetryMs = 100)
    try {
      client.startConnectionManager().unsafeRunSync()(cats.effect.unsafe.IORuntime.global)
      eventuallyConnected(client)
      val result =
        client.readMemory(0x1000L, 2).unsafeRunSync()(cats.effect.unsafe.IORuntime.global)
      assert(result.isRight, result)
      val decoded = Base64.getDecoder.decode(result.toOption.get)
      assert(decoded.toSeq == Seq(0x34.toByte, 0x12.toByte))
    } finally {
      adapterThread.join(2000L)
      server.close()
      val _ = Files.deleteIfExists(sockPath)
      val _ = Files.deleteIfExists(dir)
    }
  }

  test("LocalPipeDapClient closes streams when handshake fails") {
    val dir = Files.createTempDirectory("dap-unix-fail")
    val sockPath = dir.resolve("dap.sock")
    val server = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
    server.bind(UnixDomainSocketAddress.of(sockPath))

    val accepted = new CountDownLatch(1)
    val adapterThread = new Thread(
      () => {
        val peer = server.accept()
        accepted.countDown()
        // Accept then hang without DAP responses so initialize times out.
        try Thread.sleep(5000L)
        catch { case _: InterruptedException => () }
        finally peer.close()
      },
      "dummy-unix-fail-adapter"
    )
    adapterThread.setDaemon(true)
    adapterThread.start()

    val client =
      new LocalPipeDapClient(
        sockPath,
        dapTimeoutMs = 200,
        dapConnectTimeoutMs = 1000,
        dapConnectRetryMs = 50
      )
    val result =
      client
        .readMemory(0x1000L, 2)
        .unsafeRunSync()(cats.effect.unsafe.IORuntime.global)

    assert(result.isLeft, result)
    assert(!client.isConnected)
    assert(accepted.await(2, TimeUnit.SECONDS))
    adapterThread.interrupt()
    adapterThread.join(2000L)
    server.close()
    val _ = Files.deleteIfExists(sockPath)
    val _ = Files.deleteIfExists(dir)
  }

  test("LocalPipeDapClient serializes concurrent DAP requests") {
    val dir = Files.createTempDirectory("dap-unix-serial")
    val sockPath = dir.resolve("dap.sock")
    val server = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
    server.bind(UnixDomainSocketAddress.of(sockPath))

    val started = new CountDownLatch(1)
    val adapterThread = new Thread(
      () => {
        started.countDown()
        val peer = server.accept()
        try {
          new DummyDapStreamAdapter(
            Channels.newInputStream(peer),
            Channels.newOutputStream(peer),
            payloads,
            closeAfterReadMemory = false
          ).serveUntilClosed()
        } finally {
          peer.close()
        }
      },
      "dummy-unix-serial-adapter"
    )
    adapterThread.setDaemon(true)
    adapterThread.start()

    assert(started.await(5, TimeUnit.SECONDS))
    val client = new LocalPipeDapClient(sockPath, dapConnectRetryMs = 100)
    try {
      client.startConnectionManager().unsafeRunSync()(cats.effect.unsafe.IORuntime.global)
      eventuallyConnected(client)

      val results = (client.readMemory(0x1000L, 2), client.readMemory(0x2000L, 2)).parTupled
        .unsafeRunSync()(cats.effect.unsafe.IORuntime.global)

      assert(results._1.isRight, results._1)
      assert(results._2.isRight, results._2)
      assert(
        Base64.getDecoder.decode(results._1.toOption.get).toSeq == Seq(0x34.toByte, 0x12.toByte)
      )
      assert(
        Base64.getDecoder.decode(results._2.toOption.get).toSeq == Seq(0x78.toByte, 0x56.toByte)
      )
    } finally {
      server.close()
      adapterThread.join(2000L)
      val _ = Files.deleteIfExists(sockPath)
      val _ = Files.deleteIfExists(dir)
    }
  }

  test("DapClients.create selects LocalPipeDapClient for --dap-pipe paths") {
    val client = DapClients.create(
      dapPipe = Some(java.nio.file.Paths.get("/tmp/example.sock")),
      dapHost = "127.0.0.1",
      dapPort = 4711,
      dapTimeoutMs = 5000,
      dapContinueTimeoutMs = 30000,
      dapConnectTimeoutMs = 1000,
      dapConnectRetryMs = 5000
    )
    assert(client.isInstanceOf[LocalPipeDapClient])
  }

  private def eventuallyConnected(client: LocalPipeDapClient): org.scalatest.Assertion = {
    val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
    while (!client.isConnected && System.nanoTime() < deadline) {
      Thread.sleep(50L)
    }
    assert(client.isConnected, "expected DAP pipe session to connect")
  }
}
