package com.weirdcanada.distro.snippet

import net.liftweb.http.DispatchSnippet
import net.liftweb.util.Helpers._
import net.liftweb.util.ClearNodes
import com.weirdcanada.distro.service.Service
import net.liftweb.http.S

class PageViewSnippet(service: Service) extends DispatchSnippet {
  def dispatch = {
    case _ => {
      S.request.foreach(service.SessionManager.current.onPageView _)
      "*" #> ClearNodes
    }
  }
}