package org.weirdcanada.site.model

// weirdcanada
import org.weirdcanada.dynamicform.{
  BasicField, 
  DynamicField, 
  DynamicFieldPrimitives,
  DynamicFormFieldRenderHelpers,
  HasEmpty, 
  HasFields,
  ManyRecordField,
  RecordField
}
import DynamicFieldPrimitives.{StringPrimitive,StringPrimitiveEmpty}
import org.weirdcanada.site.lib.DBHelpers

// Lift
import net.liftweb.common.{Box, Empty, Full}
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._

// scalaz
import scalaz.Lens

// Scala
import scala.annotation.tailrec
import scala.xml.NodeSeq

// 3rd party
import org.joda.time.DateTime

case class VolunteerInterest(interest: String)

object VolunteerInterest {

  val volunteerInterestInterestLens: Lens[VolunteerInterest, String] = Lens.lensu( (vi, i) => vi.copy(interest = i), (vi) => vi.interest)
 
  val interestsSelectOptions: Seq[(String,String)] = Seq(
     ("","(select interest)"),
     ("photography","Photography"),
     ("video", "Video"),
     ("design", "Design"),
     ("write-music", "Write (Music)"),
     ("write-books", "Write (Books)"),
     ("write-grants", "Write (Grants)"),
     ("write-general", "Write (General Communications)"),
     ("write-art", "Write (Art)"),
     ("translation-french", "Translation (French)"),
     ("translation-syllabics","Translation (Syllabics)"),
     ("editing-and-proofreading","Editing & Proofreading"),
     ("admin","Administration & Data Entry"),
     ("research", "Research"),
     ("strategy", "High Level Strategizing"),
     ("app-development", "App Development"),
     ("coding", "Coding"),
     ("seo", "SEO"),
     ("bookkeeping", "Bookkeeping"),
     ("outreach", "Outreach"),
     ("show-staff", "Show Staff (Bar, Door, Merch)"),
     ("postering", "Postering"),
     ("printing-screenprinting", "Printing / Screenprinting"),
     ("driving", "Driving"),
     ("show-promoting", "Show Promoting"),
     ("volunteer-sourcing", "Volunteer Sourcing"),
     ("music-sourcing", "Music Sourcing")
   )
  
  private def interestsSelectRenderer(current: VolunteerInterest)(updateFunc: String => JsCmd): NodeSeq => NodeSeq = {
    val currentValue: Box[String] = volunteerInterestInterestLens.get(current) match {
      case "" => Empty
      case int @ _ => Full(int)
    }
    "name=volunteer-interest-interest-input" #> SHtml.ajaxSelect(interestsSelectOptions, currentValue, updateFunc)
  }
  
  implicit object VolunteerInterestRecord extends HasFields[VolunteerInterest] {
    val fields: List[DynamicField[VolunteerInterest]] = List(
      BasicField[VolunteerInterest]("volunteer-interest-interest", volunteerInterestInterestLens, Some(interestsSelectRenderer))
    )
  }

  implicit object VolunteerInterestEmpty extends HasEmpty[VolunteerInterest] {
    val empty: VolunteerInterest = VolunteerInterest("")
  }
}


