package org.weirdcanada.distro.page

import net.liftweb.http.{DispatchSnippet, SHtml}
import net.liftweb.util.Helpers._
import org.weirdcanada.distro.service.Service
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds.FocusOnLoad
import org.weirdcanada.distro.data.Account
import net.liftweb.util.Props
import org.weirdcanada.distro.data.UserRole
import net.liftweb.http.S
import scala.xml.Text
import scala.xml.NodeSeq

class CheckYourInboxPage(service: Service) extends DistroPage {
  // Redirect to the home page if someone visits this page without an active session, or has already validated their address
  val cachedAccount =
    service.SessionManager.current.accountOpt
      .filterNot(_.emailValidated.is)
      .getOrElse(S.redirectTo("/"))
  
  var emailAddress = cachedAccount.email.is
  

  def updateEmailAddress(newValue: String) = {
    if (service.AccountManager.isValidEmailAddress(newValue)) {
      emailAddress = newValue
      JsCmds.Noop
    }
    else JsCmds.Alert("Invalid email address") // TODO: something far less annoying / lazy
  }

  def resendConfirmation = {
    service.AccountManager.sendConfirmRegistrationEmail(
      service.SessionManager.current.account // Lookup the account again rather than using the cached (possibly stale) copy
        .emailValidated(false)
        .email(emailAddress)
        .saveMe
    )
    
    JsCmds.RedirectTo("/check-your-inbox")
  }
  
  def render =
    "#email-address" #> emailAddress &
    "#new-email-address" #> SHtml.ajaxText(emailAddress, updateEmailAddress _, "style" -> "border: none; outline: none; width: 100%; margin: auto; padding: 2px 5px; font-size: 20px;") &
    "#resend" #> SHtml.ajaxButton(Text("Resend Confirmation"), resendConfirmation _)
}
