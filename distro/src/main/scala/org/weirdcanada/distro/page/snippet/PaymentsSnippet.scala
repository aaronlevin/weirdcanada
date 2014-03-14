package org.weirdcanada.distro.page.snippet

import scala.xml.NodeSeq
import net.liftweb.http.DispatchSnippet
import net.liftweb.util.Helpers._
import net.liftweb.util._
import org.weirdcanada.distro.data.Account
import org.weirdcanada.distro.data.Payment
import org.weirdcanada.distro.service.Service

class PaymentsSnippet(service: Service) extends DispatchSnippet {
  sealed trait PaymentStatus
  final case object Eligible extends PaymentStatus
  final case object Ineligible extends PaymentStatus
  final case object Pending extends PaymentStatus

  val MINIMUM_BALANCE = BigDecimal(4)
  
  val paymentStatus = service.SessionManager.current.accountOpt.map { account =>
    (account.unofficialBalance.is, Payment.hasPaymentPending(account)) match {
    case (_, true) =>
      Pending
    
    case (tooLow, _) if tooLow < MINIMUM_BALANCE =>
      Ineligible

    case _ =>
      Eligible
    }}
    
  override def dispatch = {
    case "Eligible" => renderIf(Eligible) _
    case "Ineligible" => renderIf(Ineligible) _
    case "Pending" => renderIf(Pending) _
  }
  
  def renderIf(desiredStatus: PaymentStatus)(ns: NodeSeq) = service.SessionManager.current.accountOpt match { 
    case None => NodeSeq.Empty
    case _ => 
      if (paymentStatus == desiredStatus)
        ns
      else
        NodeSeq.Empty
  }
}
