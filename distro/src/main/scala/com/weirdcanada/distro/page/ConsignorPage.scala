package org.weirdcanada.distro.page

import net.liftweb.util.Helpers._
import net.liftweb.http.DispatchSnippet
import net.liftweb.util.PassThru
import org.weirdcanada.distro.service.Service
import org.weirdcanada.distro.data.Account
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import net.liftweb.http.S
import net.liftweb.common.Full
import scala.xml.NodeSeq
import org.weirdcanada.distro.service.{EmailNotValidated, BalanceTooLow, PaymentAllowed}
import scala.xml.Text

class ConsignorPage(service: Service) extends DistroPage {
  val account = service.SessionManager.current.account
  val charts = List()
  val consignedItems = account.consignedItems.toList // TODO: make sure albums are preloaded
  
  val (wasJustValidated, confirmationError) =
    (account.emailValidated.is, S.param("confirmKey")) match {
      case (false, Full(confirmKey)) =>
        if (confirmKey == account.emailConfirmationKey.is) 
          (true, None)
        else
          (false, Some("Your confirmation key is invalid or expired"))
              
      case _ =>
        (false, None)
    }
  
  def render = 
    renderRequestPayment &
    //renderCharts &
    renderConsignedItems
  
  def renderRequestPayment =
    "#request-payment" #> ((ns: NodeSeq) => 
      service.AccountManager.canRequestPayment(account) match {
        case EmailNotValidated =>
          Text("Can request payments once your email address is confirmed")
        case BalanceTooLow =>
          Text("Can request payments once your balance reaches $50")
        case PaymentAllowed =>
          ns
      }
    )
  
  def renderCharts =
    ".charts" #> charts.map(_ => PassThru) // TODO

  val dateFormatter = DateTimeFormat.forPattern("mmm dd, yyyy")
  
  def renderConsignedItems =
    "#registration-confirmed" #> (if (wasJustValidated) "*" #> PassThru else "*" #> NodeSeq.Empty) &
    "#registration-error" #> confirmationError.map(error => "registration-error-message" #> error).getOrElse("*" #> NodeSeq.Empty) &
    ".consigned-item" #> consignedItems.flatMap{ consignedItem =>
      consignedItem.album.obj.map{ album =>
        "#sku" #> album.sku.is &
        "#title" #> album.title.is &
        "#year" #> album.releaseYear.is &
        "#media-condition" #> consignedItem.mediaCondition.is.toString &
        "#cover-condition" #> consignedItem.coverCondition.is.toString &
        "#price" #> consignedItem.customerCost.is //& // TODO: when to show Wholesale?
        "#date" #> dateFormatter.print(new DateTime(consignedItem.createdAt.is))
      }
    }
}
