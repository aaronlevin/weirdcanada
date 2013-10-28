package org.weirdcanada.distro.api.shopify

import net.liftweb.json._
import org.joda.time.DateTime

object Variant {
  import org.weirdcanada.distro.util.StringExtensions._
  implicit private val formats = DefaultFormats

  def unapply(jsonString: String): Option[PersistentVariant] = {
    parseOpt(jsonString) match {
      case Some(JObject(JField("variant", variant) :: Nil)) => Some(Variant(variant))
      case _ => None // TODO: failed to parse
    }
  }
  
  def extractOptions(jv: JValue): Map[Int, String] = {
    (1 to 3)
      .map{ i =>
        i -> (jv \ "option%d".format(i)).extract[String] ?? ""
      }
      .toMap
  }
  
  def apply(jv: JValue): PersistentVariant = {
     //TODO: trace? println(jv.toString)
    
    new PersistentVariant(
      barcode = (jv \ "barcode").extract[String],
      createdAt = new DateTime((jv \ "created_at").extract[String]),
      id = (jv \ "id").extract[Long],
      options = extractOptions(jv),
      position = (jv \ "position").extract[Int],
      price = BigDecimal((jv \ "price").extract[String]),
      productId = (jv \ "product_id").extract[Long],
      sku = (jv \ "sku").extract[String],
      title = (jv \ "title").extract[String]
    )
  }
  
  // Not in love with the architecture of this... I'll just start here and refactor later if appropriate
  object ByMetafields {
    def apply(variantId: Long, metafields: Seq[Metafield]) = compactRender(
      JObject(
        JField("id", JInt(variantId)) ::
        JField("variant",
          JObject(
            Metafield.List(metafields) :: Nil
          )
        ) :: Nil
      )
    ).toString
    
    def apply(metafields: Seq[Metafield]) = compactRender(
      JObject(
        JField("variant",
          JObject(
            Metafield.List(metafields) :: Nil
          )
        ) :: Nil
      )
    ).toString
  }
  
  object List {
    def unapply(jsonString: String): Option[List[PersistentVariant]] = {
      parseOpt(jsonString) match {
        case Some(JObject(JField("variants", JArray(variants)) :: Nil)) => Some(variants.map(Variant(_)))
        case _ => None // TODO: failed to parse
      }
    }
  }
}

class Variant(
  val barcode: String,
    //"compare_at_price": null,
    //"fulfillment_service": "manual",
    //"grams": 0,
    //"inventory_management": null,
    //"inventory_policy": "deny",
  val options: Map[Int, String],
  val position: Int,
  val price: BigDecimal,
    //"requires_shipping": true,
  val sku: String,
    //"taxable": true,
  val title: String
  //val inventoryQuantity: Int
) {
  def toJson = compactRender(toJValue).toString
  
  def toJValue: JValue =
    JObject(
      List(
        "barcode" -> barcode,
        "option1" -> options.getOrElse(1, ""),
        "option2" -> options.getOrElse(2, ""),
        "option3" -> options.getOrElse(3, ""),
        "sku" -> sku,
        "title" -> title
      ).collect{
        case (field, value) if value.length > 0 =>
          JField(field, JString(value))
      } :::
      List(
        JField("position", JInt(position)),
        JField("price", JString(price.toString))
      )
    )
}

class PersistentVariant(
  val id: Long,
  barcode: String,
    //"compare_at_price": null,
  val createdAt: DateTime,
    //"fulfillment_service": "manual",
    //"grams": 0,
    //"inventory_management": null,
    //"inventory_policy": "deny",
  options: Map[Int, String],
  position: Int,
  price: BigDecimal,
  val productId: Long,
    //"requires_shipping": true,
  sku: String,
    //"taxable": true,
  title: String
    //"updated_at": "2013-09-05T10:07:19-04:00",
  //inventoryQuantity: Int
) extends Variant(barcode, options, position, price, sku, title)