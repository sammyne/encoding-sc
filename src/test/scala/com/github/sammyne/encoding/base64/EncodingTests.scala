package com.github.sammyne.encoding.base64

import org.scalatest.funsuite.AnyFunSuite
import java.nio.charset.StandardCharsets

class EncodingTests extends AnyFunSuite {
  case class encodingTest(enc: Encoding, conv: (String) => String)

  case class testpair(decoded: String, encoded: String)

  def parseHexEscapedString(s: String): String = {
    val ALPHABET: String = "0123456789abcdef"
    val unhexlify = (s: String) => {
      val b = s.toLowerCase().toCharArray()
      ((ALPHABET.indexOf(b(0)) << 4) | (ALPHABET.indexOf(b(1)) << 0)).toByte
    }

    val raw = s
      .split("""\\x""")
      .drop(1)
      .map(_.getBytes(StandardCharsets.ISO_8859_1))
      .map(v => {
        ((ALPHABET.indexOf(v(0)) << 4) | (ALPHABET.indexOf(v(1)) << 0)).toByte
      })
    new String(raw, StandardCharsets.ISO_8859_1)
  }

  def rawRef(ref: String): String = ref.replaceAll("=+$", "")

  def rawUrlRef(ref: String): String = this.rawRef(this.urlRef(ref))

  def stdRef(ref: String): String = ref

  def urlRef(ref: String): String = ref.replace('+', '-').replace('/', '_')

  val encodingTests = Array[encodingTest](
    encodingTest(STD_ENCODING, stdRef(_)),
    encodingTest(URL_ENCODING, urlRef(_)),
    encodingTest(RAW_STD_ENCODING, rawRef(_)),
    encodingTest(RAW_URL_ENCODING, rawUrlRef(_))
  )

  val pairs = Array[testpair](
    // RFC 3548 examples
    testpair(parseHexEscapedString("""\x14\xfb\x9c\x03\xd9\x7e"""), "FPucA9l+"),
    testpair(parseHexEscapedString("""\x14\xfb\x9c\x03\xd9"""), "FPucA9k="),
    testpair(parseHexEscapedString("""\x14\xfb\x9c\x03"""), "FPucAw=="),

    // RFC 4648 examples
    testpair("", ""),
    testpair("f", "Zg=="),
    testpair("fo", "Zm8="),
    testpair("foo", "Zm9v"),
    testpair("foob", "Zm9vYg=="),
    testpair("fooba", "Zm9vYmE="),
    testpair("foobar", "Zm9vYmFy"),

    // Wikipedia examples
    testpair("sure.", "c3VyZS4="),
    testpair("sure", "c3VyZQ=="),
    testpair("sur", "c3Vy"),
    testpair("su", "c3U="),
    testpair("leasure.", "bGVhc3VyZS4="),
    testpair("easure.", "ZWFzdXJlLg=="),
    testpair("asure.", "YXN1cmUu"),
    testpair("sure.", "c3VyZS4=")
  )

  test("encode") {
    for (p <- pairs; (tt, i) <- encodingTests.zipWithIndex) {
      val got = tt.enc.encodeToString(p.decoded.getBytes(StandardCharsets.ISO_8859_1))
      val expect = tt.conv(p.encoded)
      assert(expect == got, s"$i-th Encoding encode(${p.decoded})")
    }
  }
}
