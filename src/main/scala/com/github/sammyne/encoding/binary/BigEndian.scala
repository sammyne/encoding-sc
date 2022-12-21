package com.github.sammyne.encoding.binary

import java.nio.ByteBuffer

object BigEndian extends ByteOrder {
  private val backend = java.nio.ByteOrder.BIG_ENDIAN

  override def int16(buf: Array[Byte], offset: Int): Short = this.wrapBuffer(buf, offset).getShort()

  override def int32(buf: Array[Byte], offset: Int): Int = this.wrapBuffer(buf, offset).getInt()

  override def int64(buf: Array[Byte], offset: Int): Long = this.wrapBuffer(buf, offset).getLong()

  override def putInt16(buf: Array[Byte], v: Short, offset: Int): Unit = this.wrapBuffer(buf, offset).putShort(v)

  override def putInt32(buf: Array[Byte], v: Int, offset: Int): Unit = this.wrapBuffer(buf, offset).putInt(v)

  override def putInt64(buf: Array[Byte], v: Long, offset: Int): Unit = this.wrapBuffer(buf, offset).putLong(v)

  override def toString(): String = "BigEndian"

  private def wrapBuffer(b: Array[Byte], offset: Int): ByteBuffer =
    ByteBuffer.wrap(b, offset, b.length - offset).order(backend)
}
