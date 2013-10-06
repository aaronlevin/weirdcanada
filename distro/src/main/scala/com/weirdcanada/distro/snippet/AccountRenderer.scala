package com.weirdcanada.distro.snippet

import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmds
import com.weirdcanada.distro.data.Account
import com.weirdcanada.distro.page.AccountPage

trait AccountRenderer {
  def renderAccount(account: Account) = {
    ".account-link [href]" #> AccountPage.menu.calcHref(account) &
    "#name" #> account.displayName &
    "#email-address" #> account.email.is &
    "#role" #> account.role.is.toString &
    "#location" #> List(account.city.is, account.province.is, account.country.is).filterNot(_.isEmpty).mkString(", ")
  }
}

trait EditableAccountRenderer extends AccountRenderer {
  private var _account: Account = null
  
  // Helper to simplify lines below
  import scala.language.implicitConversions
  implicit def boolToJsCmd(ignored: Boolean) = JsCmds.Noop
  
  override def renderAccount(account: Account) = {
    _account = account
    super.renderAccount(account) &
    "#edit-email-address" #> SHtml.ajaxText(account.email.is, account.email.apply(_).save) &
    "#edit-first-name" #> SHtml.ajaxText(account.firstName.is, account.firstName.apply(_).save) &
    "#edit-last-name" #> SHtml.ajaxText(account.lastName.is, account.lastName.apply(_).save) &
    "#edit-address1" #> SHtml.ajaxText(account.addressLine1.is, account.addressLine1.apply(_).save) &
    "#edit-address2" #> SHtml.ajaxText(account.addressLine2.is, account.addressLine2.apply(_).save) &
    "#edit-city" #> SHtml.ajaxText(account.city.is, account.city.apply(_).save) &
    "#edit-province" #> SHtml.ajaxText(account.province.is, account.province.apply(_).save) &
    "#edit-postal-code" #> SHtml.ajaxText(account.postalCode.is, account.postalCode.apply(_).save) &
    "#edit-country" #> SHtml.ajaxText(account.country.is, account.country.apply(_).save) &
    "#edit-organization" #> SHtml.ajaxText(account.organization.is, account.organization.apply(_).save) &
    "#edit-paypal-email" #> SHtml.ajaxText(account.paypalEmail.is, account.paypalEmail.apply(_).save)
  }
}