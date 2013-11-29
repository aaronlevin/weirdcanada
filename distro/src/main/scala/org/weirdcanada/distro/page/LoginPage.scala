package org.weirdcanada.distro.page

import net.liftweb.http.DispatchSnippet
import net.liftweb.util.Helpers._
import org.weirdcanada.distro.service.Service
import net.liftweb.http.{SHtml, S}
import net.liftweb.http.js.JsCmds.FocusOnLoad
import org.weirdcanada.distro.data.Account
import net.liftweb.common.Full
import net.liftweb.http.provider.HTTPCookie
import net.liftweb.common.Empty
import net.liftweb.util.Props

class LoginPage(service: Service) extends DistroPage {
  var emailAddress = ""
  var password = ""
  
  def render =
    //"#login-form" #> SHtml.ajaxForm(onSubmit, body) &
    "@email-address" #> FocusOnLoad(SHtml.email(emailAddress, (newVal: String) => emailAddress = newVal, "type" -> "email", "class" -> "form-control")) &
    "@password" #> SHtml.password(password, password = _) &
    "#login" #> SHtml.submit("Login", () => login)
    
  def login {
    Account.findByEmailAddress(emailAddress)
      .filter(_.password.match_?(password))
      .map(service.SessionManager.current.logIn)
      .foreach(account => {
        val yearFromNow = 60 * 60 * 24 * 365
        S.addCookie(HTTPCookie("wcdid", Full(account.wcdid.is), if (Props.devMode) Empty else Full(S.hostName), Full("/"), Full(yearFromNow), Empty, Empty, Full(true)))
        S.redirectTo("/")
      })

    // TODO: log an error
    
    S.error("error", "Email address and password combination not found")
  }
}
