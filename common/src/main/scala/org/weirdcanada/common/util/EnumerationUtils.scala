package org.weirdcanada.common.util

/**
 * Utilities to deal with the worst api in scala.
 */
trait EnumerationUtils { this: Enumeration =>

  /**
   * A safer way to match enumeration values to names
   */
  def withNameOption(name: String): Option[this.Value] = try {
    Some(this.withName(name))
  } catch {
    case _: java.util.NoSuchElementException => None
  }

}
