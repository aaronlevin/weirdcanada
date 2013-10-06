package org.weirdcanada.distro.util

object StringExtensions {
  class StringExtensions(s: String) {
    def ??(default: => String) = if (s == null) default else s
  }
  
  import scala.language.implicitConversions
  implicit def coalesceString(s: String) = new StringExtensions(s)
}