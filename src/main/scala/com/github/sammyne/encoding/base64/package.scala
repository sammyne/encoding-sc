package com.github.sammyne.encoding

package object base64 {
  val STD_PADDING: Char = '='
  val NO_PADDING: Char = -1.toChar

  private[base64] val ENCODE_STD = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  private[base64] val ENCODE_URL = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

  val STD_ENCODING: Encoding = new Encoding(ENCODE_STD)
  val URL_ENCODING: Encoding = new Encoding(ENCODE_URL)

  val RAW_STD_ENCODING: Encoding = STD_ENCODING.withPadding(NO_PADDING)
  val RAW_URL_ENCODING: Encoding = URL_ENCODING.withPadding(NO_PADDING)
}
