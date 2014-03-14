package org.weirdcanada.distro.service

import net.liftweb.http.{Req, LiftResponse, LiftSession, LiftRules, Templates}
import net.liftweb.util.Props
import net.liftweb.util.Helpers._
import net.liftweb.common.{Box, Full}
import net.liftweb.db.{DB, DefaultConnectionIdentifier}
import net.liftweb.mapper.{StandardDBVendor, Schemifier}
import org.weirdcanada.distro.util.{RollingWindow, EmailFactory}
import org.weirdcanada.distro.data._
import org.weirdcanada.distro.Config
import org.weirdcanada.distro.DistroSession
import net.liftweb.db.DB1.db1ToDb
import net.liftweb.http.LiftRulesMocker.toLiftRules
import org.weirdcanada.distro.page.AccountPage
import java.util.Date


sealed trait PaymentNotAllowedReason
final case object PaymentAllowed extends PaymentNotAllowedReason
final case object EmailNotValidated extends PaymentNotAllowedReason
final case object BalanceTooLow extends PaymentNotAllowedReason
//final case object AccountOnHold extends PaymentNotAllowedReason 
//final case object PaymentsTemporarilyDisabled extends PaymentNotAllowedReason 


class AccountManager(config: Config, emailManager: EmailManager) {
  def createAccount(emailAddress: String, password: String, firstName: String, lastName: String = "", organization: String = "", address1: String = "", address2: String = "", city: String ="", province: String = "", postalCode: String = "", country: String = "", phoneNumber: String = "", paypalEmail: String = "") = {
    // TODO: move validation rules into here
    tryo {
      val newAccount =
        Account.create
          .wcdid(randomString(32)) // Used for cookie logins
          .email(emailAddress)
          .password(password)
          .firstName(firstName)
          .lastName(lastName)
          .organization(organization)
          .addressLine1(address1)
          .addressLine2(address2)
          .city(city)
          .province(province)
          .country(country)
          .postalCode(postalCode)
          .phoneNumber(phoneNumber)
          .paypalEmail(paypalEmail)
          .role(if (Props.devMode) UserRole.Admin else UserRole.UnconfirmedMember)
          .saveMe
      
      sendConfirmRegistrationEmail(newAccount)
      
      newAccount
    }
  }
  
  def sendConfirmRegistrationEmail(account: Account) {
    account.emailConfirmationKey(randomString(16)).save
    emailManager.send(account.email.is,
      ConfirmRegistrationEmail(
        "%s/?confirmKey=%s".format(config.distroEndPoint, account.emailConfirmationKey.is),
        account.firstName.is
      )
    )
  }

  /**
   * confirmRegistration is called from an early response in the SiteMap as part of the registration workflow.
   * The user will have clicked the confirm link in their email. This function tests the email and key to make sure
   * we're being called by the person who registered.
   */
  def confirmRegistration(email: String, key: String) = {
    Account
      .findByEmailAddress(email)
      .filter(_.emailConfirmationKey.is == key && key.length > 0) // match up the keys, don't allow an empty key to succeed
      .map(
        _.emailConfirmationKey("") // reset the key (this field isn't needed again until a password reset)
          .emailValidated(true)    // mark the account as validated
          .saveMe                  // save and return the account object
      )
  }
  
  def onNewAccount(account: Account) {
  }
  
  val EmailAddressRegex = """(?i)^([a-z0-9_.+-]+@[a-z0-9-]+(?:\.[a-z0-9-]+)+)$""".r // Overly loose regex. We'll send a validation email anyways.
  def isValidEmailAddress(emailAddress: String) =
    emailAddress match {
      case EmailAddressRegex(maybeValid) => true
      case _ => false
    }
  
  def canRequestPayment(account: Account): PaymentNotAllowedReason = {
    val MinimumPaymentBalance = 4

    // Not allowed if they don't have enough $$ in their account
    if (account.unofficialBalance.is < MinimumPaymentBalance) 
      return BalanceTooLow

    // TODO: check an official balance?  (or perhaps this in an offline process with a human verification step)

    PaymentAllowed
  }
  
  def requestPayment(account: Account) {
    canRequestPayment(account) match {
      case PaymentAllowed =>
        val payment =
          Payment.create
            .consignor(account)
            .requestedAt(new Date)
            .saveMe
        
        emailManager.send(config.paymentRequestEmail,
          PaymentRequestEmail(
            account.displayName,
            config.distroEndPoint + AccountPage.calcHref(Some(account.id.is)),
            account.unofficialBalance.is,
            payment.id.is
          )
        )
        
      case _ =>
        // Do nothing (shouldn't be here unless the UI allowed them after checking canRequestPayment anyhow)
    }  
  }
}
