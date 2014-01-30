package org.weirdcanada.distro.page.snippet

import net.liftweb.common.{Empty, Full, Failure}
import net.liftweb.http.{DispatchSnippet, RequestVar, SHtml}
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.sitemap.{Loc, Menu}
import net.liftweb.sitemap._
import Loc.LocGroup
import net.liftweb.util.Helpers._
import org.weirdcanada.distro.DistroSiteMapBuilder
import org.weirdcanada.distro.data.{ConsignedItem}
import ConsignedItem.ConsignedItemData
import org.weirdcanada.dynamicform.{DynamicFormCreator}
import scala.xml.{NodeSeq, Text}
import scalaz.\/
import scalaz.{\/-,-\/} // Zoidberg

class EditConsignedItemPage(consigneditemDataTuple: (ConsignedItem, ConsignedItemData)) extends DispatchSnippet with DynamicFormCreator  {

  var uploadToShopify = false
  import ConsignedItem._

  /**
   * Get consigneditem from id in `ConsignedItemData`
   */
  val consigneditem = consigneditemDataTuple._1
  val data = consigneditemDataTuple._2

  private val addErrorJs = """(function() { var x = document.getElementById('consigneditem-update'); x.className = x.className + " has-error";})();"""
  private val addSuccessJs = """(function() { var x = document.getElementById('consigneditem-update'); x.className = x.className + " has-success";})();"""
  private val removeErrorJs = """$('.has-error').removeClass('has-error');"""
  private val removeSuccessJs = """$('.has-success').removeClass('has-success');"""

  private def updateConsignedItemFunc(data: ConsignedItemData, consigneditem: ConsignedItem): JsCmd = {
    println("xxx %s".format(data))
    ConsignedItem.updateFromData(data, consigneditem) match {
      case \/-(_) =>
        JsCmds.Run(removeErrorJs + addSuccessJs) & 
        JsCmds.SetHtml("consigneditem-update-error-msg", Text("Successfully updated consigneditem with id %s".format(data.id)))
      case -\/(errorMsg) => 
        JsCmds.Run(addErrorJs) &
        JsCmds.SetHtml("consigneditem-update-error-msg", Text("%s".format(errorMsg)))
    }

  }

  /** Dynamic Field stuff
   */
  private object consigneditemState extends RequestVar[ConsignedItemData](data)
  def updateState = getUpdateAndSaveFuncForField[ConsignedItemData](consigneditemState)
  val renderFunction = renderField(consigneditemState)

  def render = renderFunction andThen {
    "@consigneditem-update-button" #> SHtml.ajaxButton(
      "Update", 
      () => {
        updateConsignedItemFunc(consigneditemState.is,consigneditem)
      }
    )
  }

  def dispatch = {
    case "render" => render
  }

}
