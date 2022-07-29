package hex.examples

import com.github.sammyne.encoding.hex

object Encode extends App {
  val src = "Hello Gopher!".getBytes()

  val dst = new Array[Byte](hex.encodedLen(src.length))
  hex.encode(dst, src)

  val got = new String(dst)
  val expect = "48656c6c6f20476f7068657221"
  assert(expect == got)
}
