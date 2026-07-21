package io.github.jacoby6000.daphttp

import java.nio.file.Path

private[daphttp] object DapClients {
  def create(
      dapPipe: Option[Path],
      dapHost: String,
      dapPort: Int,
      dapTimeoutMs: Int,
      dapContinueTimeoutMs: Int,
      dapConnectTimeoutMs: Int,
      dapConnectRetryMs: Int
  ): DapClient =
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
        new SocketDapClient(
          dapHost,
          dapPort,
          dapTimeoutMs,
          dapContinueTimeoutMs,
          dapConnectTimeoutMs,
          dapConnectRetryMs
        )
    }
}
