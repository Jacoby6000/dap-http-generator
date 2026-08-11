package io.github.jacoby6000.daphttp

import scala.util.Try

/** Parse/format DAP memory addresses as `0x…` hex strings. */
object DapAddress {
  def parse(raw: String): Option[Long] = {
    val trimmed = raw.trim.toLowerCase
    if (trimmed.isEmpty) None
    else {
      val hex = if (trimmed.startsWith("0x")) trimmed.drop(2) else trimmed
      Try(java.lang.Long.parseUnsignedLong(hex, 16)).toOption
    }
  }

  def format(address: Long): String = f"0x$address%x"
}
