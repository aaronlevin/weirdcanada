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
   * Function to save the publisher
   */
  private def savePublisherCombinator(setHtmlId: String, nodes: NodeSeq): () => JsCmd = () => {
    val data = publisherState.is
    lazy val otherPublishers = Publisher.findByName(data.name)
    lazy val newPublisherJs = Publisher.fromData(publisherState.is) match {
      case None =>
        val msg = "Incorrect type (or other error)"
        val runString = """var yadda =
          document.getElementById("publisher-type"); yadda.className =
            yadda.className + " has-error";"""
        JsCmds.Run(runString) & 
        JsCmds.SetHtml("publisher-type-error", <span class="help-block error">{msg}</span>)
      case Some(publisher) =>
        val runString = """
          var yadda = document.getElementById("publisher-save");
          yadda.className = yadda.className + " has-success"; 
          document.getElementById("publisher-save-error").innerHTML = "Successfully added publisher id %s";
        """.format(publisher.id.is)
        JsCmds.Run(runString)

    }
    if(otherPublishers.isEmpty)
      newPublisherJs
    else {
      JsCmds.Replace(setHtmlId, (
        "@publisher-save-group" #> ClearNodes & 
        "@publisher-duplicate-confirm" #> SHtml.ajaxButton(
          "Publisher Already Exists: OK?", 
          () => newPublisherJs & JsCmds.After(1500, JsCmds.SetHtml("publisher-confirm-save", Text("")))
        ) &
        "@publisher-cancel-save" #> SHtml.ajaxButton("Cancel", () => JsCmds.SetHtml("publisher-confirm-save", Text(""))
      )).apply(
        nodes
      ))
    }

  }


  /**
   * We explicitly pass the NodeSeq in to the save command so we can render the
   * button below it
   */
  def render = renderFunction andThen {
    "@publisher-save-and-confirm" #>  { (ns: NodeSeq) =>
      ("@publisher-save" #> SHtml.ajaxButton("save", savePublisherCombinator("publisher-confirm-save", ns)) &
      "@publisher-confirm-save *" #> ClearNodes)(ns)
    }
  }

  def dispatch = {
    case "render" => render
  }

}
