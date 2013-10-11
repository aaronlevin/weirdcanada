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

// Scala
import scala.xml.NodeSeq

import java.util.Locale

class VolunteerBioGenSnippet(db: DB) extends DispatchSnippet {

  // import volunteer helper methods
  import Volunteer.{searchVolunteers, renderVolunteerBio}

  // search volunteers function
  val getVolunteers = searchVolunteers(db) _ 

  // Volunteer bio template
  val volunteerBioTemplate: NodeSeq = 
    Templates("templates-hidden" :: "_volunteer_bio" :: Nil) match {
      case Full(ns) => ns
      case _ => NodeSeq.Empty
    }

  private var isEnglishBios: Boolean = true

  private def getAndSetBios: () => JsCmd = () => {

    val volunteers: Iterable[Volunteer] = getVolunteers(None, None, None, None, Nil).filter { !_.bio.descriptionEnglish.isEmpty }

    val html: NodeSeq = 
      volunteers
        .foldLeft(NodeSeq.Empty){ _ ++ renderVolunteerBio(_, isEnglishBios)(volunteerBioTemplate) }

    JsCmds.SetValById("bio-results", html.toString)

  }

  def dispatch = {
    case "searchForm" => searchForm
  }

  def searchForm = 
    "name=bio-english-button" #> SHtml.ajaxCheckbox(true, (b: Boolean) => {isEnglishBios = b; Noop}) &
    "name=get-bios-button" #> SHtml.ajaxButton("Generate Bios", getAndSetBios)
    

}
