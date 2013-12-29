package org.weirdcanada.distro.page

import net.liftweb.common.Full
import net.liftweb.http.{DispatchSnippet, S, SHtml}
import net.liftweb.http.js.{JsCmd, JsCmds}
import JsCmds.{Noop}
import net.liftweb.util.Helpers._
import net.liftweb.util.PassThru
import org.weirdcanada.common.util.DateTimeUtil
import org.weirdcanada.distro.service.{BalanceTooLow, EmailNotValidated, PaymentAllowed, Service}
import org.weirdcanada.distro.data.{Account, Sale}
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import scala.xml.{NodeSeq, Text}

class ConsignorPage(service: Service) extends DistroPage {

  /**
   * Date vars for date selection
   */
  var (startDate, endDate) = DateTimeUtil.thisWeek

  val account = service.SessionManager.current.account
  val charts = List()
  val consignedItems = account.consignedItems.toList // TODO: make sure albums are preloaded
  val consignedAlbums = consignedItems.map { _.album } // TODO: make this performant.
  lazy val sales: List[Sale] = Sale.getSalesByAccount(account.id.is)

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

  /**
   * Main render function
   */
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

  /**
   * Memoize the chart generation, including closing over the `startDate` and
   * `endDate` of the dates
   */
  val chartMemoize = SHtml.idMemoize( (memoize) =>
    "name=charts" #> charts.map { _ => PassThru }
  )

  //def renderCharts = chartMemoize

  val dateFormatter = DateTimeFormat.forPattern("mmm dd, yyyy")

  /**
   * Below are Ajax handler functions for updating the DateTime
   *
   * @param dates a tuple of `startDate` and `endDate`, passed in lazily as
   * we'll be passing in functions to calculate them. 
   * @returns a function `() to JsCmds` with the side-effect up mutating the
   * `startDate` and `endDate` vars.
   */
  private def dateUpdater(dates: => (DateTime, DateTime)) = () => {
    startDate = dates._1
    endDate = dates._2 
    // code to update data
    Noop
  }
  private def hiddenDatesUpdater: String => JsCmd = s => {
    DateTimeUtil.parseDoubleDate(s) match {
      case Some((newStartDate,newEndDate)) =>
        startDate = newStartDate
        endDate = newEndDate
        Noop
      case _ => Noop
    }
  }

  /**
   * Render the date selection. We allow the following date selection
   * parameters:
   * 1. All Time
   * 2. This Week
   * 3. This Month
   * 4. Custom (user-defined start and end)
   */
  def renderDateSelection = 
    "name=all-time" #> SHtml.a(dateUpdater(DateTimeUtil.allTime), Text("All Time")) &
    "name=this-week" #> SHtml.a(dateUpdater(DateTimeUtil.thisWeek), Text("This Week")) &
    "name=this-month" #> SHtml.a(dateUpdater(DateTimeUtil.thisMonth), Text("This Month")) &
    "name=custom-date" #> SHtml.hidden(hiddenDatesUpdater, "", "id" -> "double-date")

  /**
   * Render the Top 5 albums, as determined by the most amount of sales for each
   * album
   */
  //def renderTop5

  /**
   * Render the table displaying consigned items
   */
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
