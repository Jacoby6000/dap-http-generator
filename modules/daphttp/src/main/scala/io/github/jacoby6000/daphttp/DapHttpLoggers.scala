package io.github.jacoby6000.daphttp

import org.slf4j.Logger
import org.slf4j.LoggerFactory

object DapHttpLoggers {
  val dap: Logger = LoggerFactory.getLogger("io.github.jacoby6000.daphttp.dap")
  val http: Logger = LoggerFactory.getLogger("io.github.jacoby6000.daphttp.http")
  val irEmit: Logger = LoggerFactory.getLogger("io.github.jacoby6000.daphttp.ir.emit")
  val irSourceSmithy: Logger =
    LoggerFactory.getLogger("io.github.jacoby6000.daphttp.ir.source.smithy")
  val irSourceDoldecomp: Logger =
    LoggerFactory.getLogger("io.github.jacoby6000.daphttp.ir.source.doldecomp")
}
