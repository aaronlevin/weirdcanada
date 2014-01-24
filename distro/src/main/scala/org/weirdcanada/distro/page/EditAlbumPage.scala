package org.weirdcanada.distro.page.snippet

import net.liftweb.common.{Empty, Full, Failure}
import net.liftweb.http.{DispatchSnippet, RequestVar, SHtml}
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.sitemap.{Loc, Menu}
import net.liftweb.sitemap._
import Loc.LocGroup
import net.liftweb.util.Helpers._
import org.weirdcanada.distro.DistroSiteMapBuilder
import org.weirdcanada.distro.data.{Album, AlbumData}
import org.weirdcanada.dynamicform.{DynamicFormCreator}
import scala.xml.{NodeSeq, Text}
import scalaz.\/
import scalaz.{\/-,-\/} // Zoidberg

class EditAlbumPage(albumDataTuple: (Album, AlbumData)) extends DispatchSnippet with DynamicFormCreator  {

  import Album._

  /**
   * Get album from id in `AlbumData`
   */
  val album = albumDataTuple._1
  val data = albumDataTuple._2

  private val addErrorJs = """(function() { var x = document.getElementById('album-update'); x.className = x.className + " has-error";})();"""
  private val addSuccessJs = """(function() { var x = document.getElementById('album-update'); x.className = x.className + " has-success";})();"""
  private val removeErrorJs = """$('.has-error').removeClass('has-error');"""
  private val removeSuccessJs = """$('.has-success').removeClass('has-success');"""

  private def updateAlbumFunc(data: AlbumData, album: Album): JsCmd = {
    Album.updateFromData(data, album) match {
      case \/-(_) =>
        JsCmds.Run(removeErrorJs + addSuccessJs) & 
        JsCmds.SetHtml("album-update-error", Text("Successfully updated album with id %s".format(data.id)))
      case -\/(errorMsg) => 
        JsCmds.Run(addErrorJs) &
        JsCmds.SetHtml("album-update-error", Text("%s".format(errorMsg)))
    }

  }

  /** Dynamic Field stuff
   */
  private object albumState extends RequestVar[AlbumData](data)
  def updateState = getUpdateAndSaveFuncForField[AlbumData](albumState)
  val renderFunction = renderField(albumState)

  def render = renderFunction andThen {
    "@album-update-button" #> SHtml.ajaxButton("Update", () => updateAlbumFunc(albumState.is,album))
  }

  def dispatch = {
    case "render" => render
  }

}
