package org.weirdcanada.distro.util

object Parse {
  object SafeLong {
    def unapply(value: String): Option[Long] = {
      try {
        Some(value.toLong)
      }
      catch {
        case e: Exception =>
          None
      }
    }
  }
  
  object SafeInt {
    def unapply(value: String): Option[Int] = {
      try {
        Some(value.toInt)
      }
      catch {
        case e: Exception =>
          None
      }
    }
  }
}