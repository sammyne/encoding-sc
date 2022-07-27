package com.github.sammyne.encoding.hex

import org.scalatest.funsuite.AnyFunSuite

class PackageTests extends AnyFunSuite {
  case class encDecTest(enc: String, dec: Array[Byte])

  val encDecTests = Array(
    encDecTest("", Array[Byte]()),
    encDecTest("0001020304050607", Array[Byte](0, 1, 2, 3, 4, 5, 6, 7)),
    encDecTest("08090a0b0c0d0e0f", Array[Byte](8, 9, 10, 11, 12, 13, 14, 15)),
    encDecTest("f0f1f2f3f4f5f6f7", toBytes(0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7)),
    encDecTest("f8f9fafbfcfdfeff", toBytes(0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff)),
    encDecTest("67", Array[Byte]('g')),
    encDecTest("e3a1", toBytes(0xe3, 0xa1))
  )

  test("decode") {
    val decTests = this.encDecTests :+ encDecTest(
      "F8F9FAFBFCFDFEFF",
      toBytes(0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff)
    )

    for ((c, i) <- decTests.zipWithIndex) {
      val dst = new Array[Byte](decodedLen(c.enc.length()))
      decode(dst, c.enc.getBytes())
      assert(c.dec.deep == dst.deep, s"#$i")
    }
  }

  test("decodeString") {
    for ((c, i) <- encDecTests.zipWithIndex) {
      val got = decodeString(c.enc)
      assert(c.dec.deep == got.deep, s"#$i")
    }
  }

  test("encode") {
    for ((c, i) <- encDecTests.zipWithIndex) {
      val dst = new Array[Byte](encodedLen(c.dec.length))
      encode(dst, c.dec)

      assert(c.enc.getBytes().deep == dst.deep, s"#$i")
    }
  }

  test("encodeToString") {
    for ((c, i) <- encDecTests.zipWithIndex) {
      val got = encodeToString(c.dec)
      assert(c.enc == got, s"#$i")
    }
  }

  private def toBytes(x: Int*): Array[Byte] = x.map(_.toByte).toArray
}
