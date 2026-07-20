package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite

import java.net.StandardProtocolFamily
import java.net.UnixDomainSocketAddress
import java.nio.channels.Channels
import java.nio.channels.ServerSocketChannel
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.Base64
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit

class DapTransportSpec extends AnyFunSuite {
  private val payloads: Map[(Long, Int), Array[Byte]] =
    Map((0x1000L, 2) -> Array(0x34.toByte, 0x12.toByte))

  test("StreamDapSession reads memory over a persistent byte stream") {
    val clientToAdapter = new java.io.PipedOutputStream()
    val adapterFromClient = new java.io.PipedInputStream(clientToAdapter, 8192)
    val adapterToClient = new java.io.PipedOutputStream()
    val clientFromAdapter = new java.io.PipedInputStream(adapterToClient, 8192)

    val adapterThread = new Thread(
      () => new DummyDapStreamAdapter(adapterFromClient, adapterToClient, payloads).serveOne(),
      "dummy-stream-adapter"
    )
    adapterThread.setDaemon(true)
    adapterThread.start()

    val session = new StreamDapSession(clientFromAdapter, clientToAdapter)
    val result = session.readMemory(0x1000L, 2)
    assert(result.isRight)
    val decoded = Base64.getDecoder.decode(result.toOption.get)
    assert(decoded.toSeq == payloads((0x1000L, 2)).toSeq)

    clientToAdapter.close()
    adapterToClient.close()
    adapterThread.join(1000L)
  }

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
          ).serveOne()
        } finally {
          peer.close()
        }
      },
      "dummy-unix-adapter"
    )
    adapterThread.setDaemon(true)
    adapterThread.start()

    assert(started.await(5, TimeUnit.SECONDS))
    val client = LocalPipeDapClient.open(sockPath)
    try {
      val result =
        client.readMemory(0x1000L, 2).unsafeRunSync()(cats.effect.unsafe.IORuntime.global)
      assert(result.isRight, result)
      val decoded = Base64.getDecoder.decode(result.toOption.get)
      assert(decoded.toSeq == Seq(0x34.toByte, 0x12.toByte))
    } finally {
      client.close()
      adapterThread.join(2000L)
      server.close()
      val _ = Files.deleteIfExists(sockPath)
      val _ = Files.deleteIfExists(dir)
    }
  }

  test("DapTransportConfig.resource acquires and releases a Unix socket client") {
    val dir = Files.createTempDirectory("dap-unix-resource")
    val sockPath = dir.resolve("dap.sock")
    val server = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
    server.bind(UnixDomainSocketAddress.of(sockPath))

    val adapterThread = new Thread(
      () => {
        val peer = server.accept()
        try {
          new DummyDapStreamAdapter(
            Channels.newInputStream(peer),
            Channels.newOutputStream(peer),
            payloads
          ).serveOne()
        } finally {
          peer.close()
        }
      },
      "dummy-unix-resource-adapter"
    )
    adapterThread.setDaemon(true)
    adapterThread.start()

    val result = DapTransportConfig
      .resource(DapTransportConfig.LocalPipe(sockPath))
      .use { client =>
        client.readMemory(0x1000L, 2)
      }
      .unsafeRunSync()(cats.effect.unsafe.IORuntime.global)

    assert(result.isRight, result)
    adapterThread.join(2000L)
    server.close()
    val _ = Files.deleteIfExists(sockPath)
    val _ = Files.deleteIfExists(dir)
  }

  test("DapProtocol frames and parses readMemory responses") {
    val request = DapProtocol.buildReadMemoryRequest(7, 0xabcL, 4)
    assert(new String(request, StandardCharsets.UTF_8).contains(""""seq":7"""))
    assert(new String(request, StandardCharsets.UTF_8).contains("0xabc"))

    val ok = DapProtocol.parseReadMemoryResponse(
      """{"success":true,"body":{"data":"AQID"}}"""
    )
    assert(ok == Right("AQID"))

    val matched = DapProtocol.parseReadMemoryResponseForSeq(
      """{"type":"response","request_seq":7,"success":true,"body":{"data":"AQID"}}""",
      7
    )
    assert(matched.contains(Right("AQID")))

    val skipped = DapProtocol.parseReadMemoryResponseForSeq(
      """{"type":"event","event":"output"}""",
      7
    )
    assert(skipped.isEmpty)
  }
}
