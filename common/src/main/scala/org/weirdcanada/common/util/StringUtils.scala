package org.weirdcanada.common.util

object StringUtils {

  val slugRegex = """[^\p{L}-_]""".r

  def trim(s: String): String = s.dropWhile(_ == ' ').reverse.dropWhile(_ == ' ').reverse.toString

  def capitalizeFirstLetter(s: String) = 
    if( s.isEmpty)
      s
    else
      Character.toUpperCase(s(0)) + s.substring(1, s.length).toLowerCase

  def simpleSlug(s: String): String =
    slugRegex.replaceAllIn(s, "-").toLowerCase

  def formatSlug(s: String) =
    s.toLowerCase.replace("\"", "-inch").replace(" ", "-")

}
