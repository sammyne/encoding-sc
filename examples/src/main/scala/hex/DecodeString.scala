package hex.examples

import com.github.sammyne.encoding.hex

object DecodeString extends App {
  val src = "48656c6c6f20476f7068657221"

  val got = hex.decodeString(src)

  val expect = "Hello Gopher!"
  assert(expect == new String(got))
}
