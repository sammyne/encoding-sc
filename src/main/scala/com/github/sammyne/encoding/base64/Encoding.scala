package com.github.sammyne.encoding.base64

import java.util.Base64
import java.nio.charset.StandardCharsets
import java.io.ByteArrayOutputStream

class Encoding(rawAlphabet: String) {
  private val strCharset = StandardCharsets.ISO_8859_1

  private val alphabet = rawAlphabet.getBytes(strCharset)

  assert(alphabet.length == 64, "encoding alphabet is not 64-bytes long")
  assert(alphabet.find(v => (v == '\n') || (v == '\r')).isEmpty, "encoding alphabet contains newline character")

  private var (encoder, decoder, padChar: Option[Byte]) =
    if (rawAlphabet == ENCODE_URL) {
      (Base64.getUrlEncoder(), Base64.getUrlDecoder(), Some('='.toByte))
    } else {
      (Base64.getEncoder(), Base64.getDecoder(), Some('='.toByte))
    }

  private val decodeMap: Array[Byte] = {
    val out = new Array[Byte](256)
    for ((c, i) <- alphabet.zipWithIndex) {
      // hack: interpret as uint32, ref https://stackoverflow.com/a/7401635
      out(c & 0xff) = i.toByte
    }
    out
  }

  private def decode(dst: Array[Byte], src: Array[Byte]) = this.decoder.decode(dst, src)

  def decodedLen(n: Int): Int = {
    this.padChar match {
      case Some(_) => n / 4 * 3
      case None    => n * 6 / 8
    }
  }

  def decodeString(s: String): Array[Byte] = this.decoder.decode(s)

  def encode(dst: Array[Byte], src: Array[Byte]): Unit = {
    this.encoder.encode(src, dst)

    if ((rawAlphabet == ENCODE_STD) || (rawAlphabet == ENCODE_URL)) {
      return
    }

    val ell = this.encodedLen(src.length)
    for (i <- 0.until(ell)) {
      dst(i) = alphabet(this.decodeMap(dst(i) & 0xff))
    }

    val padding = this.padChar match {
      case Some(c) if (c == STD_PADDING) => return
      case Some(c)                       => c.toByte
      case None                          => return
    }

    for (i <- 0.until(ell).reverse) {
      if (dst(i) != STD_PADDING) {
        return
      }

      dst(i) = padding
    }
  }

  def encodedLen(n: Int): Int = {
    this.padChar match {
      case Some(_) => (n + 2) / 3 * 4
      case None    => (n * 8 + 5) / 6
    }
  }

  def encodeToString(src: Array[Byte]): String = {
    val buf = new ByteArrayOutputStream(this.encodedLen(src.length))
    val w = this.encoder.wrap(buf)

    w.write(src)
    // w.flush()
    w.close()

    val isStdPaddingOrNoPadding = this.padChar match {
      case Some(c) if (c != STD_PADDING) => false
      case _                             => true
    }
    if (((rawAlphabet == ENCODE_STD) || (rawAlphabet == ENCODE_URL)) && isStdPaddingOrNoPadding) {
      return buf.toString(strCharset.toString())
    }

    val padding = this.padChar.getOrElse(NO_PADDING.toByte)

    val dst = buf.toByteArray()

    for (i <- 0.until(dst.length)) {
      dst(i) = if (dst(i) != STD_PADDING) {
        alphabet(this.decodeMap(dst(i) & 0xff))
      } else {
        padding
      }
    }

    new String(dst, strCharset)
  }

  // def strict(): Encoding = {}

  def withPadding(padding: Char): Encoding = {
    // println(s"padding: $padding, ${padding.toInt}")
    assert((padding != '\r') && (padding != '\n') && ((padding < 0xff) || (padding == NO_PADDING)), "invalid padding")

    val out = new Encoding(this.rawAlphabet)

    if (padding == NO_PADDING) {
      out.encoder = this.encoder.withoutPadding()
      out.padChar = None
    } else {
      if (this.alphabet.find(_.toChar == padding).nonEmpty) {
        throw new IllegalArgumentException("padding contained in alphabet")
      }
      out.padChar = Some(padding.toByte)
    }

    out
  }
}
