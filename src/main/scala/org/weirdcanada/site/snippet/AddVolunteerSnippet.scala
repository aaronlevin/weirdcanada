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

// scalaz
import scalaz.Lens

object AddVolunteerSnippet extends DynamicFormCreator {

  private object volunteerState extends RequestVar[Volunteer](Volunteer(
    "","","","","","",Map.empty[Int,String],"","","","","",VolunteerBio("","","","")
  ))

  def updateState = getUpdateAndSaveFuncForField[Volunteer](volunteerState)

  val renderFunction = renderField(volunteerState)

  def render = renderFunction andThen
    "name=process-volunteer" #> SHtml.ajaxButton("Done", () => Noop)

}
