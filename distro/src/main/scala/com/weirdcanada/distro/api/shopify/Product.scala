package org.weirdcanada.distro.api.shopify

import net.liftweb.json._
import org.joda.time.DateTime

object Product {
  import org.weirdcanada.distro.util.StringExtensions._
  implicit private val formats = DefaultFormats

  def unapply(jsonString: String): Option[PersistentProduct] = {
    parseOpt(jsonString) match {
      case Some(JObject(JField("product", product) :: Nil)) => Some(Product(product))
      case _ => None // TODO: failed to parse
    }
  }
  
  def apply(jv: JValue): PersistentProduct = {
    new PersistentProduct(
      id = (jv \ "id").extract[Long],
      createdAt = new DateTime((jv \ "created_at").extract[String]),
      bodyHtml = (jv \ "body_html").extract[String],
      productType = (jv \ "product_type").extract[String],
      tags = (jv \ "tags").extract[String].split(",").toSet,
      title = (jv \ "title").extract[String],
      vendor = (jv \ "vendor").extract[String]
    )
  }
    
  object List {
    def unapply(jsonString: String): Option[List[Product]] = {
      parseOpt(jsonString) match {
        case Some(JObject(JField("products", JArray(products)) :: Nil)) => Some(products.map(Product(_)))
        case _ => None // TODO: failed to parse
      }
    }
  }
}

class Product(
  val bodyHtml: String,
  //val handle: String,
  //"images"
  //"options"
  val productType: String,
  //val publishedAt: DateTime,
  //val publishedScope: String,
  val tags: Set[String],
  //val templateSuffix: String,
  val title: String,
  //val updatedAt: DateTime,
  //val variants: List[Variant]
  val vendor: String
) {
  def toJson = compactRender(toJValue).toString
  
  def toJValue: JValue =
    JObject(List(
      JField("body_html", JString(bodyHtml)),
      JField("product_type", JString(productType)),
      JField("tags", JString(tags.mkString(","))),
      JField("title", JString(title)),
      JField("vendor", JString(vendor))
    ))
}

class PersistentProduct(
  val id: Long,
  val createdAt: DateTime,
  bodyHtml: String,
  productType: String,
  tags: Set[String],
  title: String,
  vendor: String
) extends Product(bodyHtml, productType, tags, title, vendor)