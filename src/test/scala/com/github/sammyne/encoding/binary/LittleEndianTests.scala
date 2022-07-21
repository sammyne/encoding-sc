package com.github.sammyne.encoding.binary

import org.scalatest.funsuite.AnyFunSuite

class LittleEndianTests extends AnyFunSuite {
  test("int16") {
    case class TestCase(buf: Array[Byte], offset: Int, expect: Short)

    val testVector = Array(
      TestCase(Array[Byte](0x12, 0x34, 0x56), 0, 0x3412),
      TestCase(Array[Byte](0x12, 0x34, 0x56), 1, 0x5634)
    )

    for ((c, i) <- testVector.zipWithIndex) {
      val got = if (c.offset != 0) {
        LittleEndian.int16(c.buf, c.offset)
      } else {
        LittleEndian.int16(c.buf)
      }

      assert(c.expect == got, s"#$i")
    }
  }
}
