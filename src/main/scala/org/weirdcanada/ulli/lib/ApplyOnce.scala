package org.weirdcanada.ulli.lib

case class ApplyOnce[A,B](func: A => B, default: B) {
  var runFunc: Boolean = true

  def apply(a: A): B = runFunc match {
    case true => runFunc = false; func(a)
    case false => default
  }
}
