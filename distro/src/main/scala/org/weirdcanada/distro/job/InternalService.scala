package org.weirdcanada.distro.job

import net.liftweb.actor.LiftActor
import net.liftweb.util.Schedule
import org.joda.time.DateTime
import org.weirdcanada.distro.util.RollingWindow
import org.weirdcanada.distro.util.StringExtensions._

object InternalService {
  sealed trait Status
  final case object Idle extends Status
  final case object Active extends Status
  final case object Stopped extends Status
  case class Error(msg: String) extends Status  
}

/*
 * InternalService is a generic service that piggybacks in the distro web service.
 * Implement the fn method in child classes to perform a job
 */
abstract class IntervalService(interval: Long, logLines: Int) extends LiftActor {
  protected def fn(): Unit
  
  import InternalService._
  
  private var status: Status = Stopped
  private var stopRequested = false
  private var errorEncountered = false
  
  case class Message(timestamp: DateTime, category: String, text: String)
  private val messages = new RollingWindow[Message](logLines)
  protected def addMessage(category: String, text: String) {
    messages.add(new Message(DateTime.now, category, text))
  }
  def getMessages = messages.toList
  
  private case object Start
  private case object Stop
  private case object Wakeup
  
  override def messageHandler = {
    case Start =>
      addMessage("service", "Starting")
      setAlarm(0)

    case Wakeup =>
      doIt
      if (!errorEncountered) {
        if (stopRequested) stop else setAlarm(interval)
      }
      
    case Stop =>
      if (!stopRequested)
        addMessage("service", "Stop requested")
      
      stopRequested = true
      if (status == Idle) {
        status = Stopped
        addMessage("service", "Stopped")
      }
  }

  def getStatus = status
  
  def start {
    this ! Start
  }
  
  def stop {
    this ! Stop
  }
  
  private def setAlarm(delay: Long) {
    stopRequested = false
    status = Idle
    Schedule(() => this ! Wakeup, delay)
  }
  
  private def doIt {
    status = Active
    try {
      fn()
    }
    catch {
      case e: Exception =>
        e.printStackTrace
        reportError(e.getMessage ?? e.toString)
    }
  }
  
  protected def reportError(msg: String): String = {
    addMessage("error", msg)
    status = Error(msg)
    errorEncountered = true
    msg
  }
}

