package com.weirdcanada.distro.snippet

import net.liftweb.util.Helpers._
import com.weirdcanada.distro.data.ConsignedItem
import net.liftweb.http.SHtml
import com.weirdcanada.distro.data.Account
import com.weirdcanada.distro.data.Album
import scala.xml.Text
import net.liftweb.http.js.JsCmds
import net.liftweb.http.js.JsCmd

trait ConsignedItemListRenderer {
  def renderConsignedItemList(list: List[ConsignedItem]) = {
    ".consigned-item" #> list.map{ item =>
      "#status" #> "TODO" & // indicate if sold, available, etc
      "#consignor" #> item.consignor.obj.map(_.displayName) &
      "#album" #> item.album.obj.map(_.title.is).getOrElse("[Untitled]") &
      "#cover-condition" #> item.coverCondition.is.toString &
      "#media-condition" #> item.mediaCondition.is.toString &
      "#notes" #> item.additionalNotes.is & // TODO: only show a truncated portion, with more detail on mouseover/click
      "#customer-cost" #> item.customerCost.is & // TODO: use a currency display formatter
      "#wholesale-cost" #> item.wholesaleCost.is
    }
  }
}

trait AddConsignedItemRenderer {
  var title = ""
  var consignor: Account = null
  var coverCondition = ""
  var mediaCondition = ""
  var customerCost = BigDecimal(0)
  var wholesaleCost = BigDecimal(0)
  var notes = ""
    
  def renderAddConsignedItem(account: Account) = {
    consignor = account
    ".add-consignment-item [onclick]" #> "$(this).parents('.consigned-item-list').find('.edit-consigned-item').slideDown(); return false;" &
    ".empty-consigned-item" #> (
      ".edit-save" #> SHtml.ajaxButton("Save Item", saveItem _) &
      ".edit-cancel" #> SHtml.a(cancel _, Text("Cancel"))
    )
  }
  
  private def reset = {
    title = ""
    coverCondition = ""
    mediaCondition = ""
    customerCost = 0
    wholesaleCost = 0
    notes = ""
    JsCmds.Noop
  }
  
  private def cancel = {
    reset &
    JsCmds.Run("$('.empty-consigned-item').slideUp();")
  }
  
  private def saveItem = {
    // TODO: logging and exception handling
    // TODO: look up the album
    consignor.consignedItems
      .append(
        ConsignedItem.create
          //.album(album)
          .consignor(consignor)
          // TODO
          //.coverCondition(ConsignedItem.Condition.withName(coverCondition))
          //.mediaCondition(mediaCondition)
          .customerCost(customerCost)
          .wholesaleCost(wholesaleCost)
          .saveMe
      ) // TODO: make sure I don't need to save the account here (or the lookup table between them)
      
    // TODO: add the new row to the top of the table
      
    reset
  }
}
