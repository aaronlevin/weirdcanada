package org.weirdcanada.site.lib

import scalaz.std.string
import scalaz.{Success, Failure}


/**
 * Fetch data out of a volunteer url, of the form:
 * .com/edit-volunteer/aaron/levin
 */
object EditVolunteerUrl {

  /**
   * The unapply method to be used for pattern matching
   * 
   * @param path the url path 
   * @return an option containing `VolunteerData`
   */
  def unapply(path: List[String]): Option[VolunteerUrlData] = {
    path match {
      case "edit-volunteer" :: firstName :: lastName :: Nil => Some(VolunteerUrlData(firstName, lastName))
      case _ => None
    }
  }
}

object EditVolunteerByIdUrl {

  /**
   * The unapply method to be used for pattern matching
   *
   * @param path the url path
   * @return an option containing VolunteerData
   */
  def unapply(path: List[String]): Option[Long] = {
    path match {
      case "edit-volunteer" :: id :: Nil =>
        string.parseLong(id) match {
          case Success(i) => Some(i)
          case _ => None
      }
      case _ => None
    }
  }
}

/**
 * Simple case class to encapsulate data parsed form the `edit-volunteer` url path.
 */
case class VolunteerUrlData(firstName: String, lastName: String)
