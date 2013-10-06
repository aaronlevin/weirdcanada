package com.weirdcanada.distro.page
import net.liftweb.util.Helpers._
import com.weirdcanada.distro.service.Service
import scala.xml.NodeSeq
import org.joda.time.Duration
import com.weirdcanada.distro.job.ShopifyClient
import net.liftweb.http.SHtml
import scala.xml.Text

class DashboardPage(service: Service, shopifyClient: ShopifyClient) extends DistroPage {
  
  // Note: Most of this class is temporary.  None of the markup belongs here.
  
  val renderShopifyClientActions = SHtml.idMemoize(memo => {
    import com.weirdcanada.distro.job.InternalService._

    "*" #> ((ns: NodeSeq) => shopifyClient.getStatus match {
      case Idle => <b>Idle</b> ++ SHtml.a(() => { shopifyClient.stop; memo.setHtml }, Text("Stop"))
      case Active => <b>Active</b> ++ SHtml.a(() => { shopifyClient.stop; memo.setHtml }, Text("Stop"))
      case Stopped | Error(_) => <b>Stopped</b> ++ SHtml.a(() => { shopifyClient.start; memo.setHtml }, Text("Start"))
    })
  })

  def render = "*" #> ((ns: NodeSeq) => {
    val upTime = Duration.millis(service.StatsManager.getUpTime)
    val days = upTime.toStandardDays.toStandardDuration
    val hours = upTime.minus(days).toStandardHours.toStandardDuration
    val minutes = upTime.minus(days).minus(hours).toStandardMinutes.toStandardDuration
    val seconds = upTime.minus(days).minus(hours).minus(minutes).toStandardSeconds.toStandardDuration
    
    val upTimeString = "%d days, %d hours, %d minutes, %d seconds".format(days.getStandardDays, hours.getStandardHours, minutes.getStandardMinutes, seconds.getStandardSeconds)
    
    val (ninetyFive, ninetyNine) = service.StatsManager.getRequestTimes
    
    <div>
    <h2>Server Stats</h2>
    <table>
      <tr><td>Total Uptime</td><td>{upTimeString}</td></tr>
      <tr><td>95% Avg Response Time</td><td>{ninetyFive}ms</td></tr>
      <tr><td>99% Avg Response Time</td><td>{ninetyNine}ms</td></tr>
    </table>
    <h2>Active Sessions</h2>
    <table>{
      service.SessionManager.getActiveSessions.map{ case (id, session) =>
        <tr>
          <td>{id}</td>
          <td>{"%d:%02d".format(session.duration / 60000, (session.duration / 1000) % 60)}</td>
          <td>{session.ipAddress.getOrElse("")}</td>
          <td><a href={AccountPage.calcHref(session.accountId)}>{session.accountName.getOrElse("")}</a></td>
          <td>{session.requestCount}</td>
          <td>{session.lastRequestUri.getOrElse("")}</td>
          <td>{"%d:%02d".format(session.timeOnPage / 60000, (session.timeOnPage / 1000) % 60)}</td>
        </tr>
      }
    }</table>
    <h2>Shopify Client</h2>
    <div>Current Status: {renderShopifyClientActions.apply(<span><span/></span>)}</div>
    <table>
      <thead>
        <tr>
          <th>Timestamp</th>
          <th>Category</th>
          <th>Message</th>
        </tr>
      </thead>
      <tbody>{shopifyClient.getMessages.reverse.map{ msg =>
        <tr>
          <td>{msg.timestamp.toString()}</td>
          <td>{msg.category}</td>
          <td>{msg.text}</td>
        </tr>
      }}</tbody>
    </table>
    <h2>Charts</h2>
      <div id="total-sales-chart"></div><div data-lift="Chart.totalSales?divId=total-sales-chart"></div>
      <div id="sales-by-date-chart"></div><div data-lift="Chart.allSalesByDate?divId=sales-by-date-chart"></div>
      <div id="sales-by-format-chart"></div><div data-lift="Chart.allSalesByFormat?divId=sales-by-format-chart"></div>
    </div>
  })

  
  /*
   * ram, peak ram
   * avg inventory value per consignor
   * total eligible payout amount
   * newsletters sent
   * payments requested
   * new signups
   * exceptions
   */
}