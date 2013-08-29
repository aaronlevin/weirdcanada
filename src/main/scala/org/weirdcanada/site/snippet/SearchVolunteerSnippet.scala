package org.weirdcanada.site.snippet

// weirdcanada
import org.weirdcanada.site.model.{Volunteer, VolunteerBio, VolunteerInterest}

// Lift
import net.liftweb._
import http._
import js.{JsCmd, JsCmds}
import JsCmds.{After, Alert, Replace, Run, Noop}
import common._
import util.Helpers._
import net.liftweb.db.DB

// 3rd party
import org.joda.time.DateTime

import scala.xml.NodeSeq

class SearchVolunteerSnippet(db: DB) extends DispatchSnippet {

  // import volunteer helper methods
  import Volunteer.{provinceSelectOptions, searchVolunteers}
  import VolunteerInterest.interestsSelectOptions

  private val search: (Option[String],Option[String],Option[String],Option[String],Iterable[String]) => Iterable[Volunteer] = searchVolunteers(db) _ 

  private val _searchResultMemoize: MemoizeTransform = SHtml.memoize(searchResults)

  def dispatch = {
    case "searchForm" => searchForm
    case "searchButton" => searchButton
    case "searchResults" => _searchResultMemoize
  }

  private var _city: Option[String] = None
  private var _province: Option[String] = None
  private var _interests: List[String] = Nil
  private var _firstName: Option[String] = None
  private var _lastName: Option[String] = None

  private def updateScreen: JsCmd = {
    JsCmds.SetValById("volunteer-emails", search(_firstName, _lastName, _city, _province, _interests).map { _.email }.mkString(";")) &
    Replace("search-results", _searchResultMemoize.applyAgain())
  }

  private def searchForm: NodeSeq => NodeSeq = 
    "name=select-interest" #> SHtml.ajaxSelect(interestsSelectOptions, Empty, (s: String) => { if (s.isEmpty) _interests = Nil else _interests = List(s); updateScreen}) &
    "name=select-province" #> SHtml.ajaxSelect(provinceSelectOptions, Empty, (s: String) => { if (s.isEmpty) _province = None else _province = Some(s); updateScreen }) &
    "name=search-first-name" #> SHtml.ajaxText("", (s: String) => { if (s.isEmpty) _firstName = None else _firstName = Some(s); updateScreen }, "placeholder" -> "First Name") &
    "name=search-last-name" #> SHtml.ajaxText("", (s: String) => { if (s.isEmpty) _lastName = None else _lastName = Some(s); updateScreen }, "placeholder" -> "Last Name") &
    "name=search-city" #> SHtml.ajaxText("", (s: String) => { if (s.isEmpty) _city = None else _city = Some(s); updateScreen }, "placeholder" -> "City") 

  private def searchButton: NodeSeq => NodeSeq = 
    "name=search-button [onclick]" #> SHtml.onEvent( (s: String) => updateScreen )

  private def searchResults: NodeSeq => NodeSeq = 
    "name=volunteer" #> search(_firstName, _lastName, _city, _province, _interests).map { volunteer =>
      "name=volunteer-url [href]" #> volunteer.id.map { i => "/edit-volunteer/%s".format(i) } &
      "name=volunteer-first-name" #> volunteer.firstName &
      "name=volunteer-last-name" #> volunteer.lastName &
      "name=volunteer-email" #> volunteer.email
    }

}
