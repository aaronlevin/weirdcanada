package org.weirdcanada.distro.tools

import net.liftweb.json._
import net.liftweb.common.Loggable
import org.weirdcanada.distro.Config
import org.weirdcanada.distro.api.shopify.{Shopify, Metafield}
import org.weirdcanada.distro.service.DatabaseManager
import org.weirdcanada.distro.data.ConsignedItem
import scala.io.Source
import net.liftweb.common.Full
import org.weirdcanada.distro.api.shopify.Variant
import org.weirdcanada.distro.util.AnyExtensions._
import org.weirdcanada.distro.util.IdList
import org.weirdcanada.distro.api.shopify.WrapperObject

object UploadConsignedItemToShopify extends App with Loggable {
  val config = Config.fromLiftProps
  val dbManager = new DatabaseManager(config)
  val shopify = new Shopify(config)

  dbManager.connect
  //dbManager.createSchema

  // TODO: deal with code duplication
  

  val ids =
    args.toList match {
      case "-i" :: "-" :: Nil => // Read ids from stdin
        Source.stdin.getLines.map(_.toLong)
        
      case "-i" :: IdList(ids) :: Nil =>
        Iterator(ids : _*)
        
      case _ =>
        println("Required parameters: -i <-|id1[,id2,...]>")
        println("  -i -              Reads ids from stdin, one line at a time")
        println("  -i <id1[,idN]*>   Reads ids from a comma-separated string ")
        Iterator.empty
    }

  ids.map(id => (id, ConsignedItem.findByKey(id)))
    .foreach{
      _ match {
        case (_, Full(consignedItem)) =>
          apply(consignedItem)
          
        case (id, _) =>
          println("Failed to find consigned item with id %s".format(id))
      }
    }
  
  def apply(consignedItem: ConsignedItem) = (new UploadConsignedItemToShopify(consignedItem, shopify)).upload
}

class UploadConsignedItemToShopify(consignedItem: ConsignedItem, shopify: Shopify) {
  def shopifyVariantFromConsignedItem = {
    val albumProp = consignedItem.album.obj.dmap("") _
    
    new Variant(
      barcode = "",
      options = List(
        1 -> consignedItem.guid.is // 1 -> consignedItem.consignor.obj.dmap("Default")(_.displayName)
        //2 -> consignedItem.mediaCondition.is.toString,
        //3 -> consignedItem.coverCondition.is.toString
      ).toMap,
      position = 1,
      price = consignedItem.customerCost.is,
      sku = albumProp(_.sku.is),
      title = albumProp(_.title.is)
    )
  }
  
  /*
  def shopifyVariantFromConsignedItem = {
    Metafield(
      "guid",
      consignedItem.guid.is,
      "weirdcanada"
    )
  }
  */
  
  def upload = {
    for {
      album <- consignedItem.album.obj ?~ "Consigned item %s has no album".format(consignedItem.id.is)
      //metafield = shopifyVariantFromConsignedItem
      variant = shopifyVariantFromConsignedItem
    }
    yield {
      val productId =
        album.shopifyId.is match {
          case 0 =>
            // Upload the product first
            val product = (new UploadAlbumToShopify(album, shopify)).upload
            product.id
            
          case productId => productId
        }
      
      //shopify.addProductVariant(productId, List(metafield)) |>
      shopify.addProductVariant(productId, variant) |>
        (pv => {
          println("Created Shopify variant #%s from consigned item #%s (%s)".format(pv.id, consignedItem.id.is, consignedItem.guid.is))
          
          // Now add metafield to connect the Shopify item back to the Consigned Item (I was asked to not use SKU field for this purpose)
          val metafield = Metafield("guid", consignedItem.guid.is, "weirdcanada")
          shopify.addVariantMetafields(pv.id, metafield :: Nil)
        })
    }
  }
}
