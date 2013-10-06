package org.weirdcanada.distro.api.shopify

import java.io.{BufferedReader, InputStreamReader}
import java.net.{URL, HttpURLConnection}
import net.liftweb.util.Helpers.{base64Encode, urlEncode}
import org.weirdcanada.distro.Config
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import java.io.DataOutputStream
import net.liftweb.json.JsonAST.{JValue, JObject, JField, compactRender}
import net.liftweb.common.Loggable
import net.liftweb.json.JsonAST.JInt

/**
 * Used to wrap a new object for transport to Shopify.
 * e.g. add a product to the store:
 * we have a product object p = { ... } but need to send
 * { "product": { ... } }
 * So use WrapperObject("product", p)
 */
case class WrapperObject(name: String, value: JValue) {
  def toJValue = JObject(JField(name, value) :: Nil)
  def toJson = compactRender(toJValue)
}

/**
 * Used to wrap an updated object for transport to Shopify.
 * e.g. update a product in the store:
 * we have a product object p = { ... } and id = X but need to send
 * { "product": { ..., "id": X } }
 * So use WrapperObjectWithId("product", p, X)
 * p gets injected with X and then wrapped for delivery
 */
case class WrapperObjectWithId(name: String, value: JValue, id: Long) {
  def toJValue = JObject(JField(name, value ++ JField("id", JInt(id))) :: Nil)
  def toJson = compactRender(toJValue)
}

class Shopify(config: Config) extends Loggable {
  private def toQueryString(options: Seq[(String, Any)]) = {
    options.toList match {
      case Nil => ""
      case list => "?" + list.map{ case (name, value) => "%s=%s".format(name, urlEncode(value.toString)) }.mkString("&")
    }
  }

  private def makeHttpRequest(path: String, options: Seq[(String, Any)], fnPrepConnection: HttpURLConnection => Unit): String = {
    try {
      val url = new URL(config.shopifyEndPoint + path + toQueryString(options))
      
      logger.trace(url.toString)
      
      val conn = url.openConnection.asInstanceOf[HttpURLConnection]
      val userpass = config.shopifyApiKey + ":" + config.shopifyPassword
      conn.setRequestProperty ("Authorization", "Basic " + base64Encode(userpass.getBytes))
      fnPrepConnection(conn)
      
      val rd = new BufferedReader(new InputStreamReader(conn.getInputStream))
      var result = ""
      while (rd.readLine match {
        case null => false
        case line => result += line; true
      }){}
      rd.close

      // TODO:trace(result)
      result
    }
    catch {
      case e: Exception =>
        // TODO: log.error("Failed to get %s".format(config.shopifyEndPoint + path), e)
        throw e
    }
  }
  
  private def get(path: String, options: Seq[(String, Any)]): String = {
    makeHttpRequest(path, options, conn => {
      conn.setRequestMethod("GET")
    })
  }

  private def makeHttpRequestWithBody(verb: String, path: String, body: String, options: Seq[(String, Any)]): String = {
    makeHttpRequest(path, options, conn => {
      conn.setRequestMethod(verb)
      
      logger.trace(body)

      val payload = body.getBytes("UTF-8")
      conn.addRequestProperty("Content-Type", "application/json; charset=utf-8")
      conn.addRequestProperty("Content-Length", payload.length.toString)
      conn.setDoOutput(true);
    
      val writer = new DataOutputStream(conn.getOutputStream)
      writer.write(payload)
      writer.flush
      writer.close
    })
  }

  private def post(path: String, body: String, options: Seq[(String, Any)] = Seq.empty) =
    makeHttpRequestWithBody("POST", path, body, options)

  private def put(path: String, body: String, options: Seq[(String, Any)] = Seq.empty) =
    makeHttpRequestWithBody("PUT", path, body, options)

  
  /*
  def getTransactions(orderId: Long, sinceIdOpt: Option[Long] = None): List[Transaction] = {
    sinceIdOpt.map(sinceId =>
      get("/admin/orders/%s/transactions.json?since_id=%s", orderId, sinceId)
    )
    .getOrElse(
      get("/admin/orders/%s/transactions.json", orderId)
    )
  }
  */
  
  def addProduct(product: Product) = {
    post("/admin/products.json", WrapperObject("product", product.toJValue).toJson) match {
      case Product(result) => result
      case _ => sys.error("Failed to add product: %s".format(product.toString))
    }
  }
  
  def updateProduct(productId: Long, product: Product) = {
    post("/admin/products/%s.json".format(productId), WrapperObjectWithId("product", product.toJValue, productId).toJson) match {
      case Product(result) => result
      case _ => sys.error("Failed to update product %s: %s".format(productId, product.toString))
    }
  }
  

  private val orderOptions: (String, Any) =
    "fields" -> "confirmed,created_at,currency,financial_status,fulfillment_status,gateway,id,taxes_included,test,token,total_price,total_tax,order_number,line_items,shipping_address,billing_address,customer"

  def getOrders(options: (String, Any)*): List[Order] = {
    get("/admin/orders.json", options :+ orderOptions) match {
      case Order.List(orders) => orders.reverse // I want to see oldest in the list first
      case _ => sys.error("Failed to parse orders")
    }
  }
  
  
  private val variantOptions: (String, Any) =
    "fields" -> "barcode,created_at,id,option1,option2,option3,position,price,product_id,sku,title"

  def getProductVariants(productId: Long, options: (String, Any)*): List[PersistentVariant] = {
    get("/admin/products/%s/variants.json".format(productId), options :+ variantOptions) match {
      case Variant.List(variants) => variants
      case _ => sys.error("Failed to parse variants for product %s".format(productId))
    }
  }
  
  def addProductVariant(productId: Long, variant: Variant, options: (String, Any)*): PersistentVariant = {
    post("/admin/products/%s/variants.json".format(productId), WrapperObject("variant", variant.toJValue).toJson) match {
      case Variant(result) => result
      case _ => sys.error("Failed to add product %s variant: %s".format(productId, variant.toString))
    }
  }
  
  
  private val metafieldOptions: (String, Any) =
    "fields" -> "created_at,id,key,value,value_type,namespace"
  
  def getVariantMetafields(variantId: Long, options: (String, Any)*): List[Metafield] = {
    get("/admin/variants/%s/metafields.json".format(variantId), options :+ metafieldOptions) match {
      case Metafield.List(metafields) => metafields
      case _ => sys.error("Failed to parse metafields for variant: %s".format(variantId))
    }
  }  
  
  def getProductMetafields(productId: Long, options: (String, Any)*): List[Metafield] = {
    get("/admin/products/%s/metafields.json".format(productId), options :+ metafieldOptions) match {
      case Metafield.List(metafields) => metafields
      case _ => sys.error("Failed to parse metafields for product: %s".format(productId))
    }
  }
  
  def addProductMetafield(productId: Long, metafield: Metafield) = {
    post("/admin/products/%s/metafields.json", WrapperObject("metafield", metafield.toJValue).toJson) match {
      case Metafield(result) => result
      case _ => sys.error("Failed to add metafield to product: %s".format(productId))
    }
  }
 
  def addVariantMetafields(variantId: Long, metafields: Seq[Metafield]) = {
    put("/admin/variants/%s.json".format(variantId), Variant.ByMetafields(variantId, metafields)) match {
      case Variant(result) => result
      case _ => sys.error("Failed to update metafields on variant: %s".format(variantId))
    }
  }
}
