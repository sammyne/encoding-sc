package com.github.sammyne.encoding

import scala.collection.mutable.IndexedSeqView

package object hex {
  private val ALPHABET = "0123456789abcdef".getBytes()

  /**
    * Reports an attempt to decode an odd-length input using decode or decodeString.
    */
  val EXCEPTION_LENGTH = new Exception("encoding/hex: odd length hex string")

  def decode(
      dst: Array[Byte],
      src: Array[Byte],
      dstFrom: Int = 0,
      srcFrom: Int = 0,
      srcUntil: Int = -1
  ): Unit = {
    val srcView = src.view.slice(srcFrom, srcUntil.max(src.length))
    if (srcView.length % 2 != 0) {
      throw this.EXCEPTION_LENGTH
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

  /**
   * Encodes src into slice of src into dst. encode implements hexadecimal encoding.
   *
   * @param dst
   *   the output buffer to put encoded byte
   * @param src
   *   the input buffer to encode
   * @param dstFrom
   *   the output offset in dst to start writing
   * @param srcFrom
   *   the input offset in src to start reading
   * @param srcUntil
   *   the input exclusively upper bound index in src to stop reading
   */
  def encode(
      dst: Array[Byte],
      src: Array[Byte],
      dstFrom: Int = 0,
      srcFrom: Int = 0,
      srcUntil: Int = -1
  ): Unit = {
    val srcView = src.view.slice(srcFrom, srcUntil.max(src.length))
    val dstView = dst.view.slice(dstFrom, dst.length)

    for (i <- 0.until(srcView.length)) {
      val (c, j) = (srcView(i), i * 2)
      dstView(j) = this.ALPHABET((c & 0xff) >> 4)
      dstView(j + 1) = this.ALPHABET(c & 0x0f)
    }
  }

  def encodeToString(src: Array[Byte], srcFrom: Int = 0, srcUntil: Int = -1): String = {
    val srcView = src.view.slice(srcFrom, srcUntil.max(src.length))

    val out = new StringBuilder(this.encodedLen(srcView.length))
    for (c <- srcView) {
      val (a, b) = (this.ALPHABET((c & 0xff) >> 4), this.ALPHABET(c & 0x0f))
      out.append(a.toChar)
      out.append(b.toChar)
    }

    out.toString()
  }

  def encodedLen(n: Int): Int = n * 2

  private def mapToByte(s: Byte): Byte = {
    val out = if ((s >= '0') && (s <= '9')) {
      s - '0'
    } else if ((s >= 'a') && (s <= 'f')) {
      s - 'a' + 10
    } else if ((s >= 'A') && (s <= 'F')) {
      s - 'A' + 10
    } else {
      throw new InvalidByteException(s)
    }

    out.toByte
  }
}
