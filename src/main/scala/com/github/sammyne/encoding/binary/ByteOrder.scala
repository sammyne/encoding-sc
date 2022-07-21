package com.github.sammyne.encoding.binary

trait ByteOrder {
  def int16(buf: Array[Byte], offset: Int = 0): Short
  def int32(buf: Array[Byte], offset: Int = 0): Int
  def int64(buf: Array[Byte], offset: Int = 0): Long

  def putInt16(buf: Array[Byte], v: Short, offset: Int = 0): Unit
  def putInt32(buf: Array[Byte], v: Int, offset: Int = 0): Unit
  def putInt64(buf: Array[Byte], v: Long, offset: Int = 0): Unit
}
