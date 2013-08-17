package org.weirdcanada.site.snippet

// weirdcanada
import org.weirdcanada.dynamicform.DynamicFormCreator
import org.weirdcanada.site.model.{Volunteer, VolunteerBio}

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

class AddVolunteerSnippet(db: DB) extends DynamicFormCreator with DispatchSnippet {

  // import volunteer helper methods
  import Volunteer.insertIntoDB

  // insertion function
  val insertVolunteerDB = insertIntoDB(db) _

  def dispatch = {
    case "render" => render
  }

  private object volunteerState extends RequestVar[Volunteer](Volunteer(
    //"Aaron","","","","","",Map.empty[Int,String],"","","","",new DateTime,VolunteerBio("COOL GUY","","","","","")
    "Aaron","","","","","",Map(0 -> "cool", 1 -> "neat"),"","","","",new DateTime,VolunteerBio("COOL GUY","","","","","")
  ))

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



}
