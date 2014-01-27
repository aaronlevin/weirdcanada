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
import org.weirdcanada.distro.api.shopify.{Shopify}
import org.weirdcanada.distro.data.{Account, ConsignedItem, UserRole}
import org.weirdcanada.distro.tools.UploadConsignedItemToShopify
import ConsignedItem.ConsignedItemData
import org.weirdcanada.distro.service.Service
import org.weirdcanada.dynamicform.{DynamicFormCreator, HasEmpty}
import scala.xml.{NodeSeq, Text}

class AddConsignedItemPage(service: Service) extends DynamicFormCreator with DispatchSnippet {

  import ConsignedItem._

  lazy val shopify = new Shopify(service.config)
  var uploadToShopify = false


  /** Dynamic Field stuff
   */
  private object consignedItemState extends RequestVar[ConsignedItemData](implicitly[HasEmpty[ConsignedItemData]].empty)
  def updateState = getUpdateAndSaveFuncForField[ConsignedItemData](consignedItemState)
  val renderFunction = renderField(consignedItemState)

  /**
   * Function to save the consigneditem
   */
  private def saveConsignedItemCombinator(setHtmlId: String, nodes: NodeSeq): () => JsCmd = () => {
    val data = consignedItemState.is
    lazy val otherConsignedItems = ConsignedItem.findDuplicates(data)
    lazy val newConsignedItemJs = ConsignedItem.fromData(consignedItemState.is) match {
      case None =>
        val msg = "Incorrect type (or other error)"
        val runString = """var yadda =
          document.getElementById("consigneditem-type"); yadda.className =
            yadda.className + " has-error";"""
        JsCmds.Run(runString) & 
        JsCmds.SetHtml("consigneditem-type-error", <span class="help-block error">{msg}</span>)
      case Some(consignedItem) =>
        if( uploadToShopify )
          (new UploadConsignedItemToShopify(consignedItem, shopify)).upload

        val runString = """
          var yadda = document.getElementById("consigneditem-save");
          yadda.className = yadda.className + " has-success"; 
          document.getElementById("consigneditem-save-error").innerHTML = "Successfully added Consigned Item id %s. Was it uploaded to shopify? %s";
        """.format(consignedItem.id.is, uploadToShopify)
        JsCmds.Run(runString)

    }
    if(otherConsignedItems.isEmpty)
      newConsignedItemJs
    else {
      JsCmds.Replace(setHtmlId, (
        "@consigneditem-save-group" #> ClearNodes & 
        "@consigneditem-duplicate-confirm" #> SHtml.ajaxButton(
          "ConsignedItem Already Exists: OK?", 
          () => newConsignedItemJs & JsCmds.After(1500, JsCmds.SetHtml("consigneditem-confirm-save", Text("")))
        ) &
        "@consigneditem-cancel-save" #> SHtml.ajaxButton("Cancel", () => JsCmds.SetHtml("consigneditem-confirm-save", Text(""))
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
    "@consigneditem-upload-to-shopify" #> SHtml.ajaxCheckbox(false, uploadToShopify = _) &
    "@consigneditem-save-and-confirm" #>  { (ns: NodeSeq) =>
      ("@consigneditem-save" #> SHtml.ajaxButton("save", saveConsignedItemCombinator("consigneditem-confirm-save", ns)) &
      "@consigneditem-confirm-save *" #> ClearNodes)(ns)
    }
  }

  def dispatch = {
    case "render" => render
  }

}
