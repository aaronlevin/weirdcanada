import net.liftweb.json._
import net.liftweb.common.Loggable
import org.weirdcanada.distro.Config
import org.weirdcanada.distro.api.shopify.{Shopify, Metafield, Product}
import org.weirdcanada.distro.service.DatabaseManager
import org.weirdcanada.distro.util.NullEmailFactory
import org.weirdcanada.distro.data.ConsignedItem

object ConnectConsignedItemToShopifyVariant extends App with Loggable {

  //
  //
  //   Note: This class has basically turned into a playground for adhoc testing
  //
  //   Refer to UploadConsignedItemToShopify for the code that this class was supposed to be originally
  //
  //
  
  val config = Config.fromLiftProps
  val dbManager = new DatabaseManager(config)
  val shopify = new Shopify(config)

  def listVariants(productId: Long) = {
    shopify.getProductVariants(productId)
      .foreach(v => logger.info(v.toString))
  }
  
  
  // Album ==> Product
  // ConsignedItem ==> Product Variant

  
  
  val newProduct = new Product("<b>Test</b> Product (<small>best test ever!</small>)", "Test Product", Set("test", "fake", "development"), "Test Product 1", "Test & Test Co.")
  
  val product = shopify.addProduct(newProduct)
  
  println(product)
  
//    val productId = 148034691
//    val variantId = 338149049//354002475

//  listVariants(148034691)
//Variant(null,2013-07-22T22:07:16.000-04:00,338149049,Map(1 -> Default Title, 2 -> , 3 -> ),1,10.00,148034691,xxx-100,Default Title)
//Variant(null,2013-08-30T22:16:31.000-04:00,354002475,Map(1 -> Phil's Copy, 2 -> , 3 -> ),2,12.50,148034691,PHIL001,Phil's Copy)

  
        /*
    val metafield = Metafield("guid", "blahblah", "weirdcanada")
    
    try {
      val variant = shopify.addVariantMetafields(variantId, metafield :: Nil)
      logger.info("Success")
      logger.info(variant)
    }
    catch {
      case e: Exception =>
        logger.error("Failed to add metafield to variant with id %s".format(variantId))
        e.printStackTrace
    }

*/
    
    //println(shopify.getProductVariants(productId))

    
    
/* TODO  
  dbManager.connect
  dbManager.createSchema

  val variantId = 0
  val consignedItemId = 0
  
  ConsignedItem.findByKey(consignedItemId)
    .map( consignedItem => {
      val metafield = Metafield("guid", consignedItem.guid.is, "weirdcanada")
      
      try {
        val variant = shopify.addVariantMetafields(variantId, metafield :: Nil)
        logger.info("Success")
        logger.info(variant)
      }
      catch {
        case e: Exception =>
          logger.error("Failed to add metafield to variant with id %s".format(variantId))
      }
    })
    .getOrElse(
      logger.error("Failed to find ConsignedItem with id: %s".format(consignedItemId))
    )
*/
}
