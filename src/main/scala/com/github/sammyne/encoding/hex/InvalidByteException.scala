package com.github.sammyne.encoding.hex

case class InvalidByteException(b: Byte) extends Exception(s"encoding/hex: invalid byte: '${b.toChar}'") with Equals {
  override def canEqual(that: Any): Boolean = that.isInstanceOf[InvalidByteException]

  override def equals(that: Any): Boolean = that match {
    case that: InvalidByteException => that.canEqual(this) && this.b == that.b
    case _                          => false
  }
}
