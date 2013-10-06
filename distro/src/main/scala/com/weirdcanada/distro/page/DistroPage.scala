package org.weirdcanada.distro.page

import net.liftweb.util.Helpers._
import net.liftweb.http.DispatchSnippet
import net.liftweb.util.PassThru
import net.liftweb.util.CssSel

abstract class DistroPage extends DispatchSnippet {
  override def dispatch = {
    case _ => render
  }
  
  def render: CssSel
}