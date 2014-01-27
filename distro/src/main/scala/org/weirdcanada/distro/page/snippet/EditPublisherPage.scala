package org.weirdcanada.distro.page.snippet

import net.liftweb.common.{Empty, Full, Failure}
import net.liftweb.http.{DispatchSnippet, RequestVar, SHtml}
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.sitemap.{Loc, Menu}
import net.liftweb.sitemap._
import Loc.LocGroup
import net.liftweb.util.Helpers._
import org.weirdcanada.distro.DistroSiteMapBuilder
import org.weirdcanada.distro.data.{Publisher, PublisherData}
import org.weirdcanada.dynamicform.{DynamicFormCreator}
import scala.xml.{NodeSeq, Text}
import scalaz.\/
import scalaz.{\/-,-\/} // Zoidberg

class EditPublisherPage(publisherDataTuple: (Publisher, PublisherData)) extends DispatchSnippet with DynamicFormCreator  {

  import Publisher._

  /**
   * Get publisher from id in `PublisherData`
   */
  val publisher = publisherDataTuple._1
  val data = publisherDataTuple._2

  private val addErrorJs = """(function() { var x = document.getElementById('publisher-update'); x.className = x.className + " has-error";})();"""
  private val addSuccessJs = """(function() { var x = document.getElementById('publisher-update'); x.className = x.className + " has-success";})();"""
  private val removeErrorJs = """$('.has-error').removeClass('has-error');"""
  private val removeSuccessJs = """$('.has-success').removeClass('has-success');"""

  private def updatePublisherFunc(data: PublisherData, publisher: Publisher): JsCmd = {
    Publisher.updateFromData(data, publisher) match {
      case \/-(_) =>
        JsCmds.Run(removeErrorJs + addSuccessJs) & 
        JsCmds.SetHtml("publisher-update-error", Text("Successfully updated publisher with id %s".format(data.id)))
      case -\/(errorMsg) => 
        JsCmds.Run(addErrorJs) &
        JsCmds.SetHtml("publisher-update-error", Text("%s".format(errorMsg)))
    }

  }

  /** Dynamic Field stuff
   */
  private object publisherState extends RequestVar[PublisherData](data)
  def updateState = getUpdateAndSaveFuncForField[PublisherData](publisherState)
  val renderFunction = renderField(publisherState)

  def render = renderFunction andThen {
    "@publisher-update-button" #> SHtml.ajaxButton("Update", () => updatePublisherFunc(publisherState.is,publisher))
  }

  def dispatch = {
    case "render" => render
  }

}
