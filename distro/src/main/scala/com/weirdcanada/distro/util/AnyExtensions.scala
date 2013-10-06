package com.weirdcanada.distro.util

object AnyExtensions {
  class AnyExtensions[T](t: T) {
    def |>(fn: T => Any): T = {
      fn(t)
      t
    }
  }
  
  import scala.language.implicitConversions
  implicit def usePreviousResult[T](t: T) = new AnyExtensions[T](t)
}