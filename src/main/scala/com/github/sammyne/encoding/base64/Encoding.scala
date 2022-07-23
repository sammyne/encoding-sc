package com.github.sammyne.encoding.base64

import java.util.Base64

class Encoding(alphabet: String) {
  private var (encoder, decoder) = if (alphabet == ENCODE_STD) {
    (Base64.getEncoder(), Base64.getDecoder())
  } else if (alphabet == ENCODE_URL) {
    (Base64.getUrlEncoder(), Base64.getUrlDecoder())
  } else {
    throw new IllegalArgumentException("unsupported alphabet")
  }

  private var padChar: Option[Char] = None

  def decode(dst: Array[Byte], src: Array[Byte]) = this.decoder.decode(dst, src)

  def decodedLen(n: Int): Int = {
    this.padChar match {
      case Some(_) => n / 4 * 3
      case None    => n * 6 / 8
    }
  }

  def decodeString(s: String): Array[Byte] = this.decoder.decode(s)

  def encode(dst: Array[Byte], src: Array[Byte]) = this.encoder.encode(src, dst)

  def encodedLen(n: Int): Int = {
    this.padChar match {
      case Some(_) => (n + 2) / 3 * 4
      case None    => (n * 8 + 5) / 6
    }
  }

  def encodeToString(src: Array[Byte]): String = this.encoder.encodeToString(src)

  // def strict(): Encoding = {}

  def withPadding(padding: Char): Encoding = {
    val out = new Encoding(this.alphabet)

    if (padding == -1.toChar) {
      out.encoder = this.encoder.withoutPadding()
      out.padChar = None
    } else {
      out.padChar = Some(padding)
    }

    out
  }
}
