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

class AddVolunteerSnippet(db: DB) extends DynamicFormCreator with DispatchSnippet {

  def dispatch = {
    case "render" => render
  }

  private object volunteerState extends RequestVar[Volunteer](Volunteer(
    "","","","","","",Map.empty[Int,String],"","","","","",VolunteerBio("","","","")
  ))

  def updateState = getUpdateAndSaveFuncForField[Volunteer](volunteerState)

  val renderFunction = renderField(volunteerState)

  def render = renderFunction andThen
    "name=process-volunteer" #> SHtml.ajaxButton("Done", () => Noop)

}
