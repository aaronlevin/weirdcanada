package org.weirdcanada.distro.page

import net.liftweb.common.Full
import net.liftweb.http.{DispatchSnippet, S, SHtml}
import net.liftweb.http.js.{JsCmd, JsCmds}
import JsCmds.{Noop, Replace, Run}
import net.liftweb.util.{ClearClearable, ClearNodes, Helpers, PassThru}
import Helpers._
import org.weirdcanada.common.util.DateTimeUtil
import org.weirdcanada.distro.service.{BalanceTooLow, EmailNotValidated, PaymentAllowed, Service}
import org.weirdcanada.distro.data.{Account, Album, Sale}
import Album.Type._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import scala.xml.{NodeSeq, Text}

class ConsignorPage(service: Service) extends DispatchSnippet {

  /**
   * Date vars for date selection
   */
  private var (startDate, endDate) = DateTimeUtil.allTime
  private var currentDateSelectId = "li-all-time"
  private var format = "all"
  private var currentFormatSelectId = "li-format-all"
  private var currentSalesHeader = "All Releases"

  /**
   * The account of the user viewing the consignor page.
   */
  private val account = service.SessionManager.current.account

  /**
   * What we call the user when they land on the page.
   */
  private val accountNameHeader =
    if(account.organization.is.isEmpty)
      "%s %s".format(account.firstName.is, account.lastName.is)
    else
      account.organization.is

  /**
   * The items the user / account has consigned with the wyrd distro
   */
  val consignedItems = account.consignedItems.toList // TODO: make sure albums are preloaded

  /**
   * Sales for the current account. We load up all the sales in memory as
   * they're likely to be small and this saves us from hitting the datatbase too
   * often.
   *
   * This is lazy as we may not actually need to call this if the user just
   * registered or has zero consigned items.
   */
  lazy val sales: List[Sale] = Sale.getSalesByAccount(account.id.is)

  /**
   * when a user visits this page, they may or may not have been validated or
   * may have encountered a confirmation error.
   */
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
   * Dispatch
   */
  override def dispatch = {
    case "renderRequestPayment" => renderRequestPayment
    case "renderAccountConfirmation" => renderAccountConfirmation
    case "renderDateSelection" => renderDateSelection
    case "renderFormatSelection" => renderFormatSelection
    case "renderConsignedItems" => renderConsignedItems
    case "renderAccountHeader" => "name=account-name" #> accountNameHeader
    case "renderChart" => "name=sales-charts" #> { 
      if(consignedItems.isEmpty || sales.isEmpty) {
        (_: NodeSeq) =>
          <div style="text-align:center;">
          <h3>Welcome to the consignment page!</h3>{
            if(consignedItems.isEmpty)
              <p>Unfortunately you haven't consigned any items with us yet, so we don't have any delicious data for you! Please email <a href="mailto:sell@weirdcanada.com">sell@weirdcanada.com</a> to start consigning with us!</p>
            else
              <p>Unfortunately you haven't sold any items yet, so we don't have any delicious data for you :( Never fear! We'd like to help! Please visit <a href="http://distro.weirdcanada.com/faq">this page</a> for some tips!</p>
          }
          <p>(psst! here is what the dashboard will look like when you get some sales!)</p>
          <img src="http://weird-canada-images.s3.amazonaws.com/distro-screencap.png" width="300"/>
        </div>
      } else
        chartMemoize
    }
  }

  /**
   * Render the module that flashes to the user that their account has been
   * confirmed.
   */
  def renderAccountConfirmation =
    "#registration-confirmed" #> (if (wasJustValidated) "*" #> PassThru else "*" #> NodeSeq.Empty) &
    "#registration-error" #> confirmationError.map(error => "registration-error-message" #> error).getOrElse("*" #> NodeSeq.Empty) 

  /**
   * Render the request payment buttons
   */
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
  val chartMemoize = SHtml.idMemoize( (outer) => {
    lazy val saleItems = Sale.filter(sales, format, startDate, endDate)

    lazy val bestSeller: Option[Sale] = 
      saleItems
        .groupBy { _.consignedItem.is }
        .values
        .toList
        .sortBy { - _.length }
        .headOption
        .flatMap { _.headOption }

    lazy val totalSales = saleItems.length
    lazy val amountOwed = saleItems.foldLeft(BigDecimal(0.0)){ _ + _.paidToConsignor.is }
    lazy val topCities: List[(String, String)] =
      saleItems
        .groupBy { _.city.is }
        .values
        .toList
        .sortBy { - _.length }
        .flatMap { _.headOption.map { s => (s.city.is, s.province.is) } }
        .take(5)

    "name=total-sales-header *" #> currentSalesHeader &
    "name=best-seller [src]" #> {
      for {
        best <- bestSeller
        consignedItem <- best.consignedItem.obj.toOption
        album <- consignedItem.album.obj.toOption
      } yield album.imageUrl.is 
    } &
    "name=total-sales-count *" #> saleItems.length &
    "name=total-sales-owed" #> amountOwed &
    "name=top-selling-cities-list" #> topCities.map { case (city, province) =>
      "%s, %s".format(city, province)
    } 
  })


  val dateFormatter = DateTimeFormat.forPattern("mmm dd, yyyy")

  /**
   * Below are Ajax handler functions for updating the DateTime. There is 
   * some state on the client side that needs to be handled here, mainly
   * pertaining to the "active filter" class.
   *
   * @param dates a tuple of `startDate` and `endDate`, passed in lazily as
   * we'll be passing in functions to calculate them. 
   * @returns a function `() to JsCmds` with the side-effect up mutating the
   * `startDate` and `endDate` vars.
   */
  private def dateUpdater(listId: String)(dates: => (DateTime, DateTime)) = () => {
    startDate = dates._1
    endDate = dates._2 
    val jsCmd = """
      document.getElementById("%s").className = "";
      document.getElementById("%s").className = "statsFilter-active";
    """.format(currentDateSelectId, listId)

    // Update current selected li element.
    currentDateSelectId = listId

    Run(jsCmd) &
    Replace("sales-charts", chartMemoize.applyAgain())
    // code to update data

  }
  private def hiddenDatesUpdater: String => JsCmd = s => {
    DateTimeUtil.parseDoubleDate(s) match {
      case Some((newStartDate,newEndDate)) =>
        startDate = newStartDate
        endDate = newEndDate
        currentDateSelectId = "li-custom-date"
        Replace("sales-charts", chartMemoize.applyAgain())
      case _ => Noop
    }
  }

  /**
   * Given a format type, update the client UI, and update the internal format
   * state.
   *
   * Format types are:
   * - all
   * - lp
   * - cd
   * - tape
   * - digital
   *
   * @returns a function from `Unit` to `JsCmd`
   */
  private def formatUpdater(formatType: String) = () => {

    val jsCmd = """
      document.getElementById("%s").className= "";
      document.getElementById("li-format-%s").className = "statsFilter-active";
    """.format(currentFormatSelectId,formatType)


    currentFormatSelectId = "li-format-%s".format(formatType)
    currentSalesHeader = formatType match {
      case "all" => "All Releases"
      case "lp" => "Vinyl Releses"
      case "cd" => "CDs"
      case "tape" => "Cassettes"
      case "digital" => "Digital"
      case _ => "Other Releases"
    }

    Run(jsCmd) &
    Replace("sales-charts", chartMemoize.applyAgain())
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
    "name=all-time" #> SHtml.a(dateUpdater("li-all-time")(DateTimeUtil.allTime), Text("All Time")) &
    "name=this-week" #> SHtml.a(dateUpdater("li-this-week")(DateTimeUtil.thisWeek), Text("This Week")) &
    "name=this-month" #> SHtml.a(dateUpdater("li-this-month")(DateTimeUtil.thisMonth), Text("This Month")) &
    "name=custom-date" #> SHtml.ajaxText("jerry hello", hiddenDatesUpdater, "id" -> "double_date")

  /**
   * Render the format selection. We allow the following format selection
   * parameters:
   *
   * 1. all
   * 2. LP (refers to all formats of vinyl //TODO: update this.
   * 3. cd
   * 4. tapes
   * 5. digital
   */
  def renderFormatSelection = 
    "name=format-all" #> SHtml.a(formatUpdater("all"), Text("All")) &
    "name=format-lp" #> SHtml.a(formatUpdater("lp"), Text("lp")) &
    "name=format-cd" #> SHtml.a(formatUpdater("cd"), Text("cd")) &
    "name=format-tape" #> SHtml.a(formatUpdater("tape"), Text("tape")) &
    "name=format-digital" #> SHtml.a(formatUpdater("digital"), Text("digital"))

  /**
   * Render the table displaying consigned items
   */
  def renderConsignedItems =
    consignedItems match {
      case Nil =>
       ".consigned-items" #> Nil
      case _ =>
        ".consigned-item" #> consignedItems.flatMap{ consignedItem =>
          consignedItem.album.obj.map{ album =>
            "@consigned" #> consignedItem.quantity.is &
            "@sold" #> 1 &
            "@remaining" #> 2 &
            "@price" #> "%s // %s".format(consignedItem.customerCost.is, consignedItem.wholesaleCost.is) &
            "@sku" #> consignedItem.guid &
            "@image [src]" #> album.imageUrl &
            "@artist" #> album.artists.map { _.name.is }.mkString { " // " } &
            "@title" #> album.title.is &
            "@format" #> album.formatTypeString
          }
      } &
      ClearClearable
    }
}
