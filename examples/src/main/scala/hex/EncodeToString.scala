package hex.examples

import com.github.sammyne.encoding.hex

object EncodeToString extends App {
    val src = "Hello".getBytes()
    val encodedStr = hex.encodeToString(src)
    println(encodedStr)

    // output:
    // 48656c6c6f
}
