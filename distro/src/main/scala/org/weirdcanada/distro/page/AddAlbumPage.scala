package org.weirdcanada.distro.page

import net.liftweb.http.{DispatchSnippet, SHtml}
import net.liftweb.util.ClearNodes
import net.liftweb.util.Helpers._
import net.liftweb.http.RequestVar
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds.FocusOnLoad
import net.liftweb.util.Props
import net.liftweb.http.S
import net.liftweb.common.{Full, Failure}
import net.liftweb.http.provider.HTTPCookie
import net.liftweb.common.Empty
import org.weirdcanada.common.util.{Country, Province}
import org.weirdcanada.distro.api.shopify.Shopify
import org.weirdcanada.distro.data.{Album, AlbumData}
import org.weirdcanada.distro.service.Service
import org.weirdcanada.distro.tools.UploadAlbumToShopify
import org.weirdcanada.dynamicform.{DynamicFormCreator, HasEmpty}
import scala.xml.{NodeSeq, Text, Unparsed}

class AddAlbumPage(service: Service) extends DynamicFormCreator with DispatchSnippet {

  import Album._

  lazy val shopify = new Shopify(service.config)
  var uploadToShopify = false

  /** Dynamic Field stuff
   */
  private object albumState extends RequestVar[AlbumData](implicitly[HasEmpty[AlbumData]].empty)
  def updateState = getUpdateAndSaveFuncForField[AlbumData](albumState)
  val renderFunction = renderField(albumState)


  /**
   * Function to save album and potentially upload it up to Shopify
   */
  private def saveAlbumCombinator(setHtmlId: String, nodes: NodeSeq): () => JsCmd = () => {
    val data = albumState.is
    lazy val otherAlbums = Album.findByTitle(data.title)
    lazy val newAlbumJs = Album.fromData(albumState.is) match {
      case None =>
        JsCmds.Alert("There was an error :(")
      case Some(album) =>
        if(uploadToShopify)
          (new UploadAlbumToShopify(album,shopify)).upload

        val runString = """
          var yadda = document.getElementById("album-save");
          yadda.className = yadda.className + " has-success"; 
          document.getElementById("album-save-error").innerHTML = "Successfully added album id %s. Was it uploaded to Shopify? %s;"
        """.format(album.id.is, uploadToShopify)
        JsCmds.Run(runString)

    }
    if(otherAlbums.isEmpty)
      newAlbumJs
    else {
      JsCmds.Replace(setHtmlId, (
        "@album-save-group" #> ClearNodes & 
        "@album-duplicate-confirm" #> SHtml.ajaxButton(
          "Album Already Exists: OK?", 
          () => newAlbumJs & JsCmds.After(1500, JsCmds.SetHtml("album-confirm-save", Text("")))
        ) &
        "@album-cancel-save" #> SHtml.ajaxButton("Cancel", () => JsCmds.SetHtml("album-confirm-save", Text(""))
      )).apply(
        nodes
      ))
    }

  }


  /**
   * We explicitly pass the NodeSeq in to the save command so we can render the
   * button below it
   */
  def render = renderFunction andThen
    "@album-upload-to-shopify" #> SHtml.ajaxCheckbox(false, uploadToShopify = _) &
    "@album-save-and-confirm" #> { (ns: NodeSeq) => 
      ("@album-save" #> SHtml.ajaxButton("save and upload", saveAlbumCombinator("album-confirm-save", ns)) &
      "@album-confirm-save *" #> ClearNodes)(ns)
    }


  def dispatch = {
    case "render" => render   
  }

}
