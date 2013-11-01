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

import scala.xml.{NodeSeq, Unparsed}

class AddVolunteerSnippet(db: DB, volunteer: Box[Volunteer]) extends DynamicFormCreator with DispatchSnippet {

  // import volunteer helper methods
  import Volunteer.{insertIntoDB, renderVolunteerBio}

  // insertion function
  val insertVolunteerDB = insertIntoDB(db) _

  // memoize volunteer bio snippet
  private val volunteerEnglishBioMemoize: MemoizeTransform = SHtml.memoize(bio(volunteer))
  private val volunteerFrancaisBioMemoize: MemoizeTransform = SHtml.memoize(bio(volunteer, false))

  def dispatch = {
    case "render" => render
    case "bio" => volunteerEnglishBioMemoize 
    case "bioText" => bioText
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

  private def bio(volunteer: Box[Volunteer], english: Boolean = true): NodeSeq => NodeSeq = {
    volunteer.map { v => renderVolunteerBio(v, english) } openOr { (ns: NodeSeq) => <p>Bio not available</p> }
  }

  private def bioText: NodeSeq => NodeSeq = 
    "name=get-english-bio-text" #> SHtml.ajaxButton("Get English Bio Text", () => JsCmds.SetValById("bio-textarea", volunteerEnglishBioMemoize.applyAgain().toString)) &
    "name=get-francais-bio-text" #> SHtml.ajaxButton("Get Francais Bio Text", () => JsCmds.SetValById("bio-textarea", volunteerFrancaisBioMemoize.applyAgain().toString))
}
