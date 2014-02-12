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
  def upload = {
    val option1 = 1 // Store the media format in the first option.
    
    for {
      album <- consignedItem.album.obj ?~ "Consigned item %s has no album".format(consignedItem.id.is)

    } yield {

      val variantOptions = Map(option1 -> album.format.is.toString)
      val barcode = album.barcode.is
      val position = 1 // TODO: Might not want to put a new item in first slot... maybe append to the end?
      val price = consignedItem.customerCost.is + consignedItem.markUp.is
      val sku = consignedItem.sku.is
      val title = album.titleWithArtist // TODO: show something here to distinguish from other variants?
      val markUp = consignedItem.markUp.is
      val customerCost = consignedItem.customerCost.is
      val wholesaleCost = consignedItem.wholesaleCost.is

      val productId =
        album.shopifyId.is match {
          case 0 =>
            // Upload the product first
            val product = (new UploadAlbumToShopify(album, shopify)).upload
            product.id
            
          case productId => productId
        }

      val variant = new Variant(barcode, variantOptions, position, price, sku, title)
      shopify.addProductVariant(productId, variant) |>
        (pv => {
          println("Created Shopify variant #%s from consigned item #%s (%s)".format(pv.id, consignedItem.id.is, consignedItem.sku.is))
          
          // Now add any metafields you want. (note: it should actually be possible to create the metafields in the initial call...
          // and not have to have this code here).
          //val metafields = Seq.empty[Metafield] // TODO: put here whatever metafields you want
          //shopify.addVariantMetafields(pv.id, metafields)
        }) |>
        (pv => {
          shopify.addVariantMetafields(pv.id, Seq(
            Metafield("markup", markUp.toString, "weirdcanada"), 
            Metafield("wholesalePrice", wholesaleCost.toString, "weirdcanada"),
            Metafield("customerPrice", customerCost.toString, "weirdcanada"),
            Metafield("customerPriceWithMarkUp", price.toString, "weirdcanada"),
            Metafield("wholsalePriceWithMarkUp", (wholesaleCost + markUp).toString, "weirdcanada")
          ))
        })

    }
  }
}
