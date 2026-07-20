package io.github.jacoby6000.daphttp

import cats.effect.IO
import cats.effect.Resource

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.Closeable
import java.io.InputStream
import java.io.OutputStream
import java.io.RandomAccessFile
import java.net.Socket
import java.net.StandardProtocolFamily
import java.net.UnixDomainSocketAddress
import java.nio.channels.Channels
import java.nio.channels.SocketChannel
import java.nio.file.Path

private[daphttp] trait DapClient {
  def readMemory(address: Long, sizeBytes: Int): IO[Either[String, String]]
}

private[daphttp] sealed trait DapTransportConfig

private[daphttp] object DapTransportConfig {
  final case class Tcp(host: String, port: Int) extends DapTransportConfig

  /** Local IPC path (client only). Windows `\\.\pipe\Name` or a Unix domain socket path (e.g.
    * dolphin-dap `Dolphin.General.DAPSocket=/tmp/dolphin-dap.sock`).
    */
  final case class LocalPipe(path: Path) extends DapTransportConfig

  def resource(config: DapTransportConfig): Resource[IO, DapClient] =
    config match {
      case Tcp(host, port) =>
        Resource.pure[IO, DapClient](new SocketDapClient(host, port))
      case LocalPipe(path) =>
        Resource.make(IO.blocking(LocalPipeDapClient.open(path)))(client =>
          IO.blocking(client.close())
        )
    }
}

/** Persistent DAP session over a bidirectional byte stream (pipe / Unix socket). */
private[daphttp] final class StreamDapSession(in: InputStream, out: OutputStream) {
  private val lock = new Object
  private var nextSeq = 1
  private val bufferedIn = in match {
    case already: BufferedInputStream => already
    case other                        => new BufferedInputStream(other)
  }
  private val bufferedOut = out match {
    case already: BufferedOutputStream => already
    case other                         => new BufferedOutputStream(other)
  }

  def readMemory(address: Long, sizeBytes: Int): Either[String, String] =
    lock.synchronized {
      val seq = nextSeq
      nextSeq += 1
      val request = DapProtocol.buildReadMemoryRequest(seq, address, sizeBytes)
      DapProtocol.writeFramed(bufferedOut, request)

      var result: Option[Either[String, String]] = None
      while (result.isEmpty) {
        val body = DapProtocol.readFramedMessage(bufferedIn)
        result = matchReadMemoryResponse(body, seq)
      }
      result.get
    }

  private def matchReadMemoryResponse(
      body: String,
      expectedSeq: Int
  ): Option[Either[String, String]] =
    DapProtocol.parseReadMemoryResponseForSeq(body, expectedSeq).orElse {
      // DESNOTE(jbarber, 2026-07-19): Some simple adapters omit request_seq and only speak one
      // response per request on a dedicated stream. Accept the first response-shaped message when
      // request_seq is absent so pipe/socket mocks stay easy to write.
      // See https://microsoft.github.io/debug-adapter-protocol/specification#Base_Protocol_Response
      io.circe.parser.parse(body).toOption.flatMap { json =>
        val cursor = json.hcursor
        val messageType = cursor.downField("type").as[String].toOption
        val requestSeq = cursor.downField("request_seq").as[Int].toOption
        val hasSuccess = cursor.downField("success").as[Boolean].isRight
        if (requestSeq.isEmpty && (messageType.contains("response") || hasSuccess)) {
          Some(DapProtocol.parseReadMemoryResponse(body))
        } else {
          None
        }
      }
    }
}

private[daphttp] final class SocketDapClient(host: String, port: Int) extends DapClient {
  override def readMemory(address: Long, sizeBytes: Int): IO[Either[String, String]] =
    IO.blocking {
      val socket = new Socket(host, port)
      try {
        socket.setSoTimeout(5000)
        val out = new BufferedOutputStream(socket.getOutputStream)
        val in = new BufferedInputStream(socket.getInputStream)
        val request = DapProtocol.buildReadMemoryRequest(1, address, sizeBytes)
        DapProtocol.writeFramed(out, request)
        val body = DapProtocol.readFramedMessage(in)
        DapProtocol.parseReadMemoryResponse(body)
      } finally {
        socket.close()
      }
    }.handleError(error => Left(error.getMessage))
}

private[daphttp] final class LocalPipeDapClient private (
    resources: List[Closeable],
    session: StreamDapSession
) extends DapClient
    with Closeable {
  override def readMemory(address: Long, sizeBytes: Int): IO[Either[String, String]] =
    IO.blocking(session.readMemory(address, sizeBytes))
      .handleError(error => Left(error.getMessage))

  override def close(): Unit =
    resources.foreach(_.close())
}

private[daphttp] object LocalPipeDapClient {
  def open(path: Path): LocalPipeDapClient =
    if (isWindowsNamedPipePath(path)) {
      // DESNOTE(jbarber, 2026-07-19): Connect as a client to an *existing* Windows named pipe.
      // Java can open \\.\pipe\Name via RandomAccessFile in "rw" mode; it does not create them.
      // Prefer the String constructor so device paths are not rewritten by File/NIO.
      // See https://learn.microsoft.com/en-us/windows/win32/ipc/pipe-names
      val raf = new RandomAccessFile(path.toString, "rw")
      val channel = raf.getChannel
      val in = Channels.newInputStream(channel)
      val out = Channels.newOutputStream(channel)
      new LocalPipeDapClient(List(raf), new StreamDapSession(in, out))
    } else {
      // DESNOTE(jbarber, 2026-07-19): On Linux/macOS, VS Code-style "pipe" paths are Unix domain
      // sockets. dolphin-dap listens this way via Dolphin.General.DAPSocket (AF_UNIX).
      // See https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/nio/channels/SocketChannel.html
      val channel = SocketChannel.open(StandardProtocolFamily.UNIX)
      channel.connect(UnixDomainSocketAddress.of(path))
      val in = Channels.newInputStream(channel)
      val out = Channels.newOutputStream(channel)
      new LocalPipeDapClient(List(channel), new StreamDapSession(in, out))
    }

  private def isWindowsNamedPipePath(path: Path): Boolean = {
    val normalized = path.toString.replace('/', '\\').toLowerCase
    normalized.startsWith("""\\.\pipe\""") || normalized.startsWith("""\\?\pipe\""")
  }
}
