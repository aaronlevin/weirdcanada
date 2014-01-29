package org.weirdcanada.distro.page.snippet

import net.liftweb.common.{Empty, Full, Failure}
import net.liftweb.http.{DispatchSnippet, RequestVar, SHtml}
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.sitemap.{Loc, Menu}
import net.liftweb.sitemap._
import Loc.LocGroup
import net.liftweb.util.Helpers._
import org.weirdcanada.distro.DistroSiteMapBuilder
import org.weirdcanada.distro.data.{Artist, ArtistData}
import org.weirdcanada.dynamicform.{DynamicFormCreator}
import scala.xml.{NodeSeq, Text}
import scalaz.\/
import scalaz.{\/-,-\/} // Zoidberg

class EditArtistPage(artistDataTuple: (Artist, ArtistData)) extends DispatchSnippet with DynamicFormCreator  {

  import Artist._

  /**
   * Get artist from id in `ArtistData`
   */
  val artist = artistDataTuple._1
  val data = artistDataTuple._2

  private val addErrorJs = """(function() { var x = document.getElementById('artist-update'); x.className = x.className + " has-error";})();"""
  private val addSuccessJs = """(function() { var x = document.getElementById('artist-update'); x.className = x.className + " has-success";})();"""
  private val removeErrorJs = """$('.has-error').removeClass('has-error');"""

  private def updateArtistFunc(data: ArtistData, artist: Artist): JsCmd = {
    Artist.updateFromData(data, artist) match {
      case \/-(_) => 
        JsCmds.Run(removeErrorJs + addSuccessJs) & 
        JsCmds.SetHtml("artist-update-error", Text("Successfully updated artist with id %s".format(data.id)))
      case -\/(errorMsg) => 
        JsCmds.Run(addErrorJs) &
        JsCmds.SetHtml("artist-update-error", Text("%s".format(errorMsg)))
    }

  }

  /** Dynamic Field stuff
   */
  private object artistState extends RequestVar[ArtistData](data)
  def updateState = getUpdateAndSaveFuncForField[ArtistData](artistState)
  val renderFunction = renderField(artistState)

  def render = renderFunction andThen {
    "@artist-update-button" #> SHtml.ajaxButton("Update", () => updateArtistFunc(artistState.is,artist))
  }

  def dispatch = {
    case "render" => render
  }

}
