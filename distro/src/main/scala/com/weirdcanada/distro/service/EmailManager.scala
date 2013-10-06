package org.weirdcanada.distro.service

import net.liftweb.util.Helpers._
import org.weirdcanada.distro.util.EmailFactory
import org.weirdcanada.distro.Config
import net.liftweb.http.Templates


sealed trait EmailTemplate {
  def subject: String
}
abstract class BaseEmailTemplate(val subject: String) extends EmailTemplate
case class ConfirmRegistrationEmail(confirmUrl: String, firstName: String) extends BaseEmailTemplate("Registration Confirmation")
case class PaymentRequestEmail(name: String, accountUrl: String, unofficialBalance: BigDecimal) extends BaseEmailTemplate("Payment Request")


class EmailManager(config: Config, emailFactory: EmailFactory) {
  private def get(template: String) = Templates(template.split('/').toList).openOr(sys.error("Couldn't open %s template".format(template)))
    
  private val confirmRegistrationTemplate = get("email-hidden/confirm-registration-email")
  private val paymentRequestTemplate = get("email-hidden/payment-request-email")

  
  def send(to: String, emailTemplate: EmailTemplate) = {
    val xhtml = emailTemplate match {
      case ConfirmRegistrationEmail(confirmUrl, firstName) => (
        "#confirm [href]" #> confirmUrl &
        "#first-name" #> firstName
      ).apply(confirmRegistrationTemplate)
      
      case PaymentRequestEmail(name, accountUrl, unofficialBalance) => (
        "#name" #> name &
        "#account-link [href]" #> accountUrl &
        "#balance" #> unofficialBalance
      ).apply(paymentRequestTemplate)
      
      case unhandled @ _ =>
        sys.error("Unhandled email template type: %s".format(unhandled))
    }
    
    emailFactory.send(to, config.smtpUsername, emailTemplate.subject, xhtml)
  }
}