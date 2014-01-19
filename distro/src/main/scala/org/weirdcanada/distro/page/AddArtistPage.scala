package org.weirdcanada.distro.page

import net.liftweb.http.{DispatchSnippet, SHtml}
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
import org.weirdcanada.distro.data.{Account, Artist, ArtistData, UserRole}
import org.weirdcanada.distro.service.Service
import org.weirdcanada.dynamicform.{DynamicFormCreator, HasEmpty}
import scala.xml.Text

class AddArtistPage(service: Service) extends DynamicFormCreator with DispatchSnippet {

  import Artist._

  private object artistState extends RequestVar[ArtistData](implicitly[HasEmpty[ArtistData]].empty)

  def updateState = getUpdateAndSaveFuncForField[ArtistData](artistState)

  val renderFunction = renderField(artistState)

  /**
   * Function to save the artist
   */
  val saveArtistFunc: () => JsCmd = () => {
    val data = artistState.is
    lazy val otherArtists = Artist.findByName(data.name)
    lazy val newArtist = Artist.fromData(artistState.is) match {
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
      newArtist
    else {
      JsCmds.SetHtml("artist-save-error", (
        "@newfield" #> SHtml.ajaxButton("Artist Already Exists: OK?", () => newArtist) &
        "@cancelfield" #> SHtml.ajaxButton("Cancel", () => JsCmds.SetHtml("artist-save-error", Text(""))
      )).apply(
        <span name="newfield"></span><span name="cancelfield"></span>
      ))
    }

  }

  def render = renderFunction andThen {
    "@artist-save" #> SHtml.ajaxButton("save", saveArtistFunc, "onmouseup" -> "$('.error').remove(); $('.has-error').removeClass('has-error');")
  }

  def dispatch = {
    case "render" => render
  }

}
