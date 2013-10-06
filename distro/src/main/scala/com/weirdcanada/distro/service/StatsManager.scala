package org.weirdcanada.distro.service

import org.weirdcanada.distro.Config
import org.weirdcanada.distro.util.RollingWindow
import net.liftweb.http.Req
import net.liftweb.common.Box
import net.liftweb.http.LiftResponse

class StatsManager(config: Config) {
  private val startTime = System.currentTimeMillis
  def getUpTime = System.currentTimeMillis - startTime

  val pageTimes = new RollingWindow[Int](10000, 0)
  val ajaxTimes = new RollingWindow[Int](10000, 0)
  val resourceTimes = new RollingWindow[Int](10000, 0)

  def onRequestEnd(req: Req, resp: Box[LiftResponse]) {
    val window = req.path(0) match {
      case "ajax_request" => ajaxTimes
      case "classpath" => resourceTimes // TODO: classpath is probably wrong...
      case _ => pageTimes
    }
    
    val requestDurationMS = ((System.nanoTime - req.nanoStart) / 1000000L).toInt
    window.add(requestDurationMS)
  }
  
  def getRequestTimes = {
    pageTimes.toList.sorted match {
      case Nil => (0, 0)
      case one :: nil => (one, one)
      case times =>
        val ninetyFive = times.take(times.length * 95 / 100)
        val ninetyNine = times.take(times.length * 99 / 100)
        (ninetyFive.sum / ninetyFive.length, ninetyNine.sum / ninetyNine.length)
    }
  }
}
