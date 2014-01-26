package org.weirdcanada.distro.page

import net.liftweb.common.{Box, Empty, Failure, Full}
import net.liftweb.http.{DispatchSnippet, SHtml}
import net.liftweb.util.StringHelpers.randomString
import net.liftweb.util.Helpers._
import org.weirdcanada.distro.service.{ForgotPasswordEmailTemplate, Service}
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds.FocusOnLoad
import org.weirdcanada.distro.data.Account
import net.liftweb.util.Props
import org.weirdcanada.distro.data.UserRole
import net.liftweb.http.S
import scala.xml.Text
import scala.xml.NodeSeq

class ForgotPasswordPage(service: Service) extends DispatchSnippet {

  var emailAddress = ""

  def dispatch = {
    case "render" => render
  }

  def errorJs(msg: String): JsCmd = {
    JsCmds.Run(
      """(function () { var x = document.getElementById('email').className = "form-group has-error";
      document.getElementById('email-input-error-msg').innerHTML = '%s';})()""".format(msg)
    )

  }

  def properJs(msg: String): JsCmd = {
    JsCmds.Run("""
      (function () { document.getElementById('email').className = 'form-group has-success';
        document.getElementById('email-input-error-msg').innerHTML = "%s";})()""".format(msg)
    )
  }

  def emailConfirm(send: Boolean): JsCmd = 
    service.AccountManager.isValidEmailAddress(emailAddress) match {
      case false => errorJs("What kind of weird email address is %s?".format(emailAddress))
      case _ => 
        Account.findByEmailAddress(emailAddress) match {
          case Empty => errorJs("Email does not exist. Have you registered?")
          case Failure(msg,_,_) => errorJs("Something *really* bad just happened. Contact Levin: aaron@weirdcanada.com\n%s".format(msg))
          case Full(account) => 
            if(send) {
              val newPassword = randomString(10)
              account.password(newPassword).save
              service.EmailManager.send(account.email.is, ForgotPasswordEmailTemplate(account.displayName, newPassword))
              properJs("Check your inbox for a new password!")
            } else 
              JsCmds.Noop
        }
    }

  private def render =
    "@email" #> SHtml.ajaxText("", (s: String) => { emailAddress = s;  emailConfirm(false) }) &
    "@send-email" #> SHtml.ajaxButton("Email New Password", () => emailConfirm(true) )


}
