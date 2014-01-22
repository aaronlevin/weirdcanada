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
import org.weirdcanada.distro.data.{Account, Publisher, PublisherData, UserRole}
import org.weirdcanada.distro.service.Service
import org.weirdcanada.dynamicform.{DynamicFormCreator, HasEmpty}
import scala.xml.{NodeSeq, Text}

class AddPublisherPage(service: Service) extends DynamicFormCreator with DispatchSnippet {

  import Publisher._


  /** Dynamic Field stuff
   */
  private object publisherState extends RequestVar[PublisherData](implicitly[HasEmpty[PublisherData]].empty)
  def updateState = getUpdateAndSaveFuncForField[PublisherData](publisherState)
  val renderFunction = renderField(publisherState)

  /**
   * Function to save the artist
   *
  private def saveArtistCombinator(setHtmlId: String, nodes: NodeSeq): () => JsCmd = () => {
    val data = artistState.is
    lazy val otherArtists = Artist.findByName(data.name)
    lazy val newArtistJs = Artist.fromData(artistState.is) match {
      case None =>
        val msg = "Incorrect type (or other error)"
        val runString = """var yadda =
          document.getElementById("artist-type"); yadda.className =
            yadda.className + " has-error";"""
        JsCmds.Run(runString) & 
        JsCmds.SetHtml("artist-type-error", <span class="help-block error">{msg}</span>)
      case Some(artist) =>
        val runString = """
          var yadda = document.getElementById("artist-save");
          yadda.className = yadda.className + " has-success"; 
          document.getElementById("artist-save-error").innerHTML = "Successfully added artist id %s";
        """.format(artist.id.is)
        JsCmds.Run(runString)

    }
    if(otherArtists.isEmpty)
      newArtistJs
    else {
      JsCmds.Replace(setHtmlId, (
        "@artist-save-group" #> ClearNodes & 
        "@artist-duplicate-confirm" #> SHtml.ajaxButton(
          "Artist Already Exists: OK?", 
          () => newArtistJs & JsCmds.After(1500, JsCmds.SetHtml("artist-confirm-save", Text("")))
        ) &
        "@artist-cancel-save" #> SHtml.ajaxButton("Cancel", () => JsCmds.SetHtml("artist-confirm-save", Text(""))
      )).apply(
        nodes
      ))
    }

  }*/

  /**
   * We explicitly pass the NodeSeq in to the save command so we can render the
   * button below it
   */
  def render = renderFunction 
  /*andThen {
    "@artist-save-and-confirm" #>  { (ns: NodeSeq) =>
      ("@artist-save" #> SHtml.ajaxButton("save", saveArtistCombinator("artist-confirm-save", ns)) &
      "@artist-confirm-save *" #> ClearNodes)(ns)
    }
  }*/

  def dispatch = {
    case "render" => render
  }

}
