package org.weirdcanada.distro.snippet

import net.liftweb.http.DispatchSnippet
import net.liftweb.util.Helpers._
import net.liftweb.util.ClearNodes
import org.weirdcanada.distro.service.Service
import net.liftweb.http.S
import net.liftweb.util.PassThru

class HasRoleSnippet(service: Service) extends DispatchSnippet {
  val role = service.SessionManager.current.userRole.toString.toLowerCase
  
  def dispatch = {
    case _ => {
      if (S.param("role").map(_.toLowerCase.split(',').toSet.contains(role)).getOrElse(false))
        "*" #> PassThru
      else
        "*" #> ClearNodes
    }
  }
}