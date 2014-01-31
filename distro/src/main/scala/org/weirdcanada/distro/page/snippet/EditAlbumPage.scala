package org.weirdcanada.distro.page.snippet

import net.liftweb.common.{Empty, Full, Failure}
import net.liftweb.http.{DispatchSnippet, RequestVar, SHtml}
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.sitemap.{Loc, Menu}
import net.liftweb.sitemap._
import Loc.LocGroup
import net.liftweb.util.Helpers._
import org.weirdcanada.distro.DistroSiteMapBuilder
import org.weirdcanada.distro.api.shopify.Shopify
import org.weirdcanada.distro.data.{Album, AlbumData}
import org.weirdcanada.distro.tools.UploadAlbumToShopify
import org.weirdcanada.dynamicform.{DynamicFormCreator}
import scala.xml.{NodeSeq, Text}
import scalaz.\/
import scalaz.{\/-,-\/} // Zoidberg

class EditAlbumPage(albumDataTuple: (Album, AlbumData, Shopify)) extends DispatchSnippet with DynamicFormCreator  {

  import Album._

  /**
   * Get album from id in `AlbumData`
   */
  val album = albumDataTuple._1
  val data = albumDataTuple._2
  val shopify = albumDataTuple._3

  lazy val shopifyUploader = new UploadAlbumToShopify(album, shopify)

  var uploadToShopify = false

  private val addErrorJs = """(function() { var x = document.getElementById('album-update'); x.className = x.className + " has-error";})();"""
  private val addSuccessJs = """(function() { var x = document.getElementById('album-update'); x.className = x.className + " has-success";})();"""
  private val removeErrorJs = """$('.has-error').removeClass('has-error');"""
  private val removeSuccessJs = """$('.has-success').removeClass('has-success');"""

  private def updateAlbumFunc(data: AlbumData, album: Album): JsCmd = {
    Album.updateFromData(data, album) match {
      case \/-(_) =>
        if( uploadToShopify ) { try {
          shopifyUploader.upload
          JsCmds.Run(removeErrorJs + addSuccessJs) & 
          JsCmds.SetHtml("album-update-error", Text("Successfully updated album and uploaded to shopify with id %s".format(data.id)))
        } catch { case e: java.lang.RuntimeException => 
          JsCmds.Run(removeSuccessJs + addErrorJs) &
          JsCmds.SetHtml("album-update-error", <p>Er, something terrible happened :(</p><p>{e}</p>)
        }} else {
          JsCmds.Run(removeErrorJs + addSuccessJs) & 
          JsCmds.SetHtml("album-update-error", Text("Successfully updated album with id %s".format(data.id)))
        }
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
    "@album-upload-to-shopify" #> SHtml.ajaxCheckbox(false, uploadToShopify = _) &
    "@album-update-button" #> SHtml.ajaxButton("Update", () => updateAlbumFunc(albumState.is,album))
  }

  def dispatch = {
    case "render" => render
  }

}
