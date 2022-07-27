package com.github.sammyne.encoding

import scala.collection.mutable.IndexedSeqView

package object hex {
  private val ALPHABET = "0123456789abcdef"

  def decode(
      dst: Array[Byte],
      src: Array[Byte],
      dstFrom: Int = 0,
      srcFrom: Int = 0,
      srcUntil: Int = -1
  ): Unit = {
    val srcView = src.view.slice(srcFrom, srcUntil.max(src.length))
    if (srcView.length % 2 != 0) {
      throw new IllegalArgumentException(s"bad src length: ${srcView.length}")
    }

    val dstView = dst.view.slice(dstFrom, dst.length)
    for (i <- 0.until(srcView.length, 2)) {
      val (a, b) = (mapToByte(srcView(i)), mapToByte(srcView(i + 1)))
      dstView(i / 2) = ((a << 4) | b).toByte
    }
  }

  def decodeString(s: String): Array[Byte] = {
    val src = s.getBytes()
    val out = new Array[Byte](this.decodedLen(src.length))
    this.decode(out, src)
    out
  }

  def decodedLen(x: Int): Int = x / 2

  private def mapToByte(s: Byte): Byte = {
    val out = if ((s >= '0') && (s <= '9')) {
      s - '0'
    } else if ((s >= 'a') && (s <= 'f')) {
      s - 'a' + 10
    } else if ((s >= 'A') && (s <= 'F')) {
      s - 'A' + 10
    } else {
      throw new IllegalArgumentException(s"non-hex char $s")
    }

    out.toByte
  }
}
