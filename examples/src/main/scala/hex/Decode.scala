package hex.examples

import com.github.sammyne.encoding.hex

object Decode extends App {
  val src = "48656c6c6f20476f7068657221".getBytes()

  val dst = new Array[Byte](hex.decodedLen(src.length))

  hex.decode(dst, src)

  val got = new String(dst)
  val expect = "Hello Gopher!"
  assert(expect == got)
}
