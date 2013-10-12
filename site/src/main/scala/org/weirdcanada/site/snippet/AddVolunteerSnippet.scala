package org.weirdcanada.site.snippet

// weirdcanada
import org.weirdcanada.dynamicform.DynamicFormCreator
import org.weirdcanada.site.model.{Volunteer, VolunteerBio, VolunteerInterest}

// Lift
import net.liftweb._
import http._
import js.{JsCmd, JsCmds}
import JsCmds.{After, Alert, Replace, Run, Noop}
import common._
import util.Helpers._
import net.liftweb.db.DB

// scalaz
import scalaz.Lens

// 3rd party
import org.joda.time.DateTime

import scala.xml.NodeSeq

class AddVolunteerSnippet(db: DB, volunteer: Box[Volunteer]) extends DynamicFormCreator with DispatchSnippet {

  // import volunteer helper methods
  import Volunteer.{insertIntoDB, renderVolunteerBio}

  // insertion function
  val insertVolunteerDB = insertIntoDB(db) _

  def dispatch = {
    case "render" => render
    case "bio" => bio(volunteer) 
  }

  private object volunteerState extends RequestVar[Volunteer](
    volunteer openOr {
      Volunteer(
      None,"","","","","","",Map.empty[Int,VolunteerInterest],"","","","",new DateTime,VolunteerBio("","","","","","")
      )
    }
  )

  def updateState = getUpdateAndSaveFuncForField[Volunteer](volunteerState)

  val renderFunction = renderField(volunteerState)

  def render = renderFunction andThen
    "name=process-volunteer" #> SHtml.ajaxButton("Done", () => insertVolunteer(volunteerState))

  private def insertVolunteer(v: RequestVar[Volunteer]): JsCmd = {
    val volunteer = v.is
    try {
      insertVolunteerDB(volunteer)
      Alert("Success!")
    } catch {
      case e: Exception => Alert("Issue inserting: %s".format(e))
    }
  }

  private def bio(volunteer: Box[Volunteer]): NodeSeq => NodeSeq = {
    volunteer.map { v => renderVolunteerBio(v, true) } openOr { (ns: NodeSeq) => <p>Bio not available</p> }
  }
}
