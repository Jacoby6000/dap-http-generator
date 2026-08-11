package io.github.jacoby6000.daphttp

import org.scalatest.funsuite.AnyFunSuite

final class DapAddressSpec extends AnyFunSuite {
  test("parse accepts 0x-prefixed and bare hex") {
    assert(DapAddress.parse("0x80400000").contains(0x80400000L))
    assert(DapAddress.parse("80400000").contains(0x80400000L))
    assert(DapAddress.parse("  0XABC  ").contains(0xabcL))
  }

  test("parse rejects empty and non-hex") {
    assert(DapAddress.parse("").isEmpty)
    assert(DapAddress.parse("   ").isEmpty)
    assert(DapAddress.parse("xyz").isEmpty)
  }

  test("format round-trips with parse") {
    val addr = 0x804d72e0L
    assert(DapAddress.parse(DapAddress.format(addr)).contains(addr))
  }
}
