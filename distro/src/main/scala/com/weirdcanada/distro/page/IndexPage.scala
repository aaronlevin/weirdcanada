package com.weirdcanada.distro.page

import com.weirdcanada.distro.service.Service
import net.liftweb.util.Helpers._
import net.liftweb.util.PassThru

class IndexPage(service: Service) extends DistroPage {
  def render = {
    if (service.SessionManager.current.isLoggedIn)
      ".logged-in ^^" #> PassThru
    else
      ".logged-out ^^" #> PassThru
  }
}