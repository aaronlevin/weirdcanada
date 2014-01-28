package org.weirdcanada.common.util

object StringUtils {

  def trim(s: String): String = s.dropWhile(_ == ' ').reverse.dropWhile(_ == ' ').reverse.toString

}
