package org.weirdcanada.common.util

object StringParsingUtil {

  /**
   * Typeclass that encapsulates the notion that a string can be parsed into type T
   */
  trait StringParsable[T] {
    val op: String => T
  }

  /**
   * Instances of the `StringParsable` typeclass
   */
  implicit object string2Int extends StringParsable[Int] { val op = (s: String) => s.toInt }
  implicit object string2Long extends StringParsable[Long] { val op = (s: String) => s.toLong }
  implicit object string2Double extends StringParsable[Double] { val op = (s: String) => s.toDouble }
  implicit object string2BigDecimal extends StringParsable[BigDecimal] { val op = (s: String) => BigDecimal(s) }
  implicit object string2Boolean extends StringParsable[Boolean] { val op = (s: String) => s.toBoolean }

  /**
   * Given a string try to parse it into `T`, returning None if there was
   * a failure
   */
  def safeParse[T : StringParsable](string: String): Option[T] = try {
    Some(implicitly[StringParsable[T]].op(string))
  } catch {
    case _: Throwable => None
  }

}
