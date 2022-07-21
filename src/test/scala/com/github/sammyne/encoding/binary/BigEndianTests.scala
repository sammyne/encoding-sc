package com.github.sammyne.encoding.binary

import org.scalatest.funsuite.AnyFunSuite

class BigEndianTests extends AnyFunSuite {
  test("BigEndian.int16") {
    case class TestCase(buf: Array[Byte], offset: Int, expect: Short)

    val testVector = Array(
      TestCase(Array[Byte](0x12, 0x34, 0x56), 0, 0x1234),
      TestCase(Array[Byte](0x12, 0x34, 0x56), 1, 0x3456)
    )

    for ((c, i) <- testVector.zipWithIndex) {
      val got = if (c.offset != 0) {
        BigEndian.int16(c.buf, c.offset)
      } else {
        BigEndian.int16(c.buf)
      }

      assert(c.expect == got, s"#$i")
    }
  }
}
