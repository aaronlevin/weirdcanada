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
import org.weirdcanada.distro.data.{Album, AlbumData}
import org.weirdcanada.distro.service.Service
import org.weirdcanada.dynamicform.{DynamicFormCreator, HasEmpty}
import scala.xml.{NodeSeq, Text, Unparsed}

class AddAlbumPage(service: Service) extends DynamicFormCreator with DispatchSnippet {

  import Album._

  /** Dynamic Field stuff
   */
  private object albumState extends RequestVar[AlbumData](implicitly[HasEmpty[AlbumData]].empty)
  def updateState = getUpdateAndSaveFuncForField[AlbumData](albumState)
  val renderFunction = renderField(albumState)

  /**
   * We explicitly pass the NodeSeq in to the save command so we can render the
   * button below it
   */
  def render = renderFunction

  def dispatch = {
    case "render" => render   
  }

}
