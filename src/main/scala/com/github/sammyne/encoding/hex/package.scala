package com.github.sammyne.encoding

import scala.collection.mutable.IndexedSeqView

/** Implements hexadecimal encoding and decoding. */
package object hex {
  private val ALPHABET = "0123456789abcdef".getBytes()

  /** Reports an attempt to decode an odd-length input using decode or decodeString.
    */
  val EXCEPTION_LENGTH = new Exception("encoding/hex: odd length hex string")

  /** Decodes src into dst.
    *
    * decode expects that src contains only hexadecimal characters and that src has even length.
    *
    * @param dst
    *   the output buffer to put decoded byte
    * @param src
    *   the input buffer to read byte to decode
    * @param dstFrom
    *   the output offset in dst to start writing
    * @param srcFrom
    *   the input offset in src to start reading
    * @param srcUntil
    *   the input exclusively upper bound index in src to stop reading. -1 means src.length
    * @throws EXCEPTION_LENGTH
    *   non-even length of input src slice
    * @throws InvalidByteException
    *   wrapping the first invalid byte
    */
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

  /** Returns the bytes represented by the hexadecimal string s. decodeString expects that src contains only hexadecimal
    * characters and that src has even length.
    *
    * @param s
    *   even-length hexadecimal string to decode
    * @return
    *   bytes decoded from s
    * @throws EXCEPTION_LENGTH
    *   non-even length of input src slice
    * @throws InvalidByteException
    *   wrapping the first invalid byte
    */
  def decodeString(s: String): Array[Byte] = {
    val src = s.getBytes()
    val out = new Array[Byte](this.decodedLen(src.length))
    this.decode(out, src)
    out
  }

  /** Returns the length of a decoding of x source bytes. Specifically, it returns x / 2.
    *
    * @param x
    *   length of source bytes
    * @return
    *   output length for decoding x bytes
    */
  def decodedLen(x: Int): Int = x / 2

  /** Encodes slice of src into dst using hexadecimal encoding.
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
    *   the input exclusively upper bound index in src to stop reading. -1 means src.length
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

  /** Returns the hexadecimal encoding of slice of src.
    *
    * @param src
    *   the input buffer to encode
    * @param dstFrom
    *   the output offset in dst to start writing
    * @param srcFrom
    *   the input offset in src to start reading
    * @param srcUntil
    *   the input exclusively upper bound index in src to stop reading. -1 means src.length
    * @return
    *   a hexadecimal-encoded string
    */
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

  /** Returns the length of an encoding of n source bytes. Specifically, it returns n * 2.
    *
    * @param n
    *   length of bytes to encode
    * @return
    *   length of output bytes by encoding n btyes
    */
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
