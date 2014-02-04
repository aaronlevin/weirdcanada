package org.weirdcanada.common.util

import net.liftweb.util.{ClearNodes, CssSel}
import scala.xml.NodeSeq

/**
 * Utilities for Lift
 */

object LiftUtils {

  def showIf[A](predicate: A => Boolean)(a: A)(transform: CssSel): NodeSeq => NodeSeq =
    if(predicate(a))
      transform
    else 
      ClearNodes
}
