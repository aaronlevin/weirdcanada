package org.weirdcanada.distro.page

import net.liftweb.common.{Box, Empty, Full}
import net.liftweb.http.{DispatchSnippet, SHtml}
import net.liftweb.util.StringHelpers.randomString
import net.liftweb.util.Helpers._
import org.weirdcanada.distro.service.{ForgotPasswordEmailTemplate, SessionManager, Service}
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds.FocusOnLoad
import org.weirdcanada.distro.data.Account
import net.liftweb.util.Props
import org.weirdcanada.distro.data.UserRole
import net.liftweb.http.S
import scala.xml.Text
import scala.xml.NodeSeq
import scalaz._
import Scalaz._
import scalaz.syntax._


object UpdatePasswordPage {

  /*
  implicit object jsCmdApplicative extends Semigroup[JsCmd] {
    def append(j1: JsCmd, j2: JsCmd): JsCmd = j1 & j2
  }*/
}

class UpdatePasswordPage(service: Service, account: Account) extends DispatchSnippet {

  var oldPassword = ""
  var newPassword = ""
  var confirmPassword = ""

  def errorJs(id: String, msgId: String, msg: String): JsCmd = {
    JsCmds.Run(
      """(function () { var x = document.getElementById('').className = "form-group has-error";
      document.getElementById('%s').innerHTML = '%s';})()""".format(id, msgId, msg)
    )

  }

  def properJs(id: String, msgId: String,  msg: String): JsCmd = {
    JsCmds.Run("""
      (function () { document.getElementById('update-password').className = 'form-group has-success';
        document.getElementById('update-password-error-msg').innerHTML = "Your Password has been updated!";})()""".format(id,msgId, msg)
    )
  }
  private def validateOldPassword(oldPassword: String): ValidationNel[JsCmd, JsCmd] = 
    account.password.match_?(oldPassword) match {
      case true => properJs("old-password","old-password-error-msg","").success[NonEmptyList[JsCmd]]
      case false => Failure[NonEmptyList[JsCmd], JsCmd](
        NonEmptyList(errorJs("old-password", "old-password-error-msg", "This is not your password. Try again."))
      )
    }

  private def validateBothPasswordsMatch(newPassword: String, oldPassword: String): ValidationNel[JsCmd, JsCmd] = 
    if( newPassword.equals(confirmPassword) )
      properJs("confirm-password", "confirm-password-error-msg","").success[NonEmptyList[JsCmd]]
    else
      Failure[NonEmptyList[JsCmd], JsCmd](NonEmptyList(errorJs("confirm-password", "confirm-password-error-msg", "This does not match your new password.")))

  private def redirect: ValidationNel[JsCmd, JsCmd] = 
    if( newPassword.isEmpty)
      Failure[NonEmptyList[JsCmd], JsCmd](NonEmptyList(
        errorJs("new-password", "new-password-error-msg", "New password is empty.")
      ))
    else if ( confirmPassword.isEmpty )
      Failure[NonEmptyList[JsCmd], JsCmd](NonEmptyList(
        errorJs("confirm-password", "confirm-password-error-msg", "Confirm password is empty.")
      ))
    else
        properJs("update-password", "update-password-error-msg", "Success! You will be redirected!").success[NonEmptyList[JsCmd]]

  def validate: () => JsCmd = () => (
    validateOldPassword(oldPassword) |@|
    validateBothPasswordsMatch(newPassword, oldPassword) |@|
    redirect
  ) { _ & _ & _ } match {
    case Failure(jsCmds) => jsCmds.foldLeft[JsCmd](JsCmds.Noop){ _ & _ }
    case Success(jsCmd) => 
      account.password(newPassword).saveMe
      jsCmd
  }



  def dispatch = {
    case "render" => render
  }

  private def render = 
    "@old-password" #> SHtml.ajaxText("", oldPassword = _, "type" -> "password") &
    "@new-password" #> SHtml.ajaxText("", newPassword = _, "type" -> "password") &
    "@confirm-password" #> SHtml.ajaxText("", confirmPassword = _, "type" -> "password") &
    "@update-password" #> SHtml.ajaxButton("Update Password", validate)


}
