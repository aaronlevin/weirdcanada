package org.weirdcanada.site.lib

// Weird Canada
import org.weirdcanada.site.model.{Volunteer}

// Lift
import net.liftweb.common.{Box, Empty}
import net.liftweb.http.RequestVar

trait WCRequestVar[A] {

  /**
   * A container to hold data on a per-request basis
   */
  private object requestValue extends RequestVar[Box[A]](Empty)

  /**
   * A method to set the current value
   *
   * @param a Lift's box containing a value of type `A`
   * @return nothing. This side-effects.
   */
  def setCurrentValue(a: Box[A]): Unit =  requestValue.set(a)

  /**
   * A method to return the current value
   *
   * @return the current value (of type `A`)
   */
  def currentValue: Box[A] = requestValue.is

}

/**
 * A singletone to encapsulate variables caught during requests
 */
object RequestVars {

  object ReqVolunteer extends WCRequestVar[Volunteer]

}
