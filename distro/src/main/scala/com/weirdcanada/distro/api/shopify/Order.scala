package com.weirdcanada.distro.api.shopify

import net.liftweb.json._
import org.joda.time.DateTime

object Order {
  implicit private val formats = DefaultFormats

  sealed trait FinancialStatus
  final case object Pending extends FinancialStatus
  final case object Authorized extends FinancialStatus
  final case object PartiallyPaid extends FinancialStatus
  final case object Paid extends FinancialStatus
  final case object PartiallyRefunded extends FinancialStatus
  final case object Refunded extends FinancialStatus
  final case object Voided extends FinancialStatus
  object FinancialStatus {
    def apply(jv: JValue) = {
      jv.extract[String] match {
        case "authorized" => Authorized
        case "partially_paid" => PartiallyPaid
        case "paid" => Paid
        case "partially_refunded" => PartiallyRefunded
        case "refunded" => Refunded
        case "voided" => Voided
        case unexpected @ _ => sys.error("Unexpected financial status: %s".format(unexpected))
      }
    }
  }

  sealed trait FulfillmentStatus
  final case object Fulfilled extends FulfillmentStatus
  final case object Null extends FulfillmentStatus
  final case object Partial extends FulfillmentStatus
  object FulfillmentStatus {
    def apply(jv: JValue) = {
      jv.extract[String] match {
        case "fulfilled" => Fulfilled
        case "null" | null => Null
        case "partial" => Partial
        case unexpected @ _ => sys.error("Unexpected fulfillment status: %s".format(unexpected))
      }    
    }
  }

  
  object Address {
    def getOpt(jv: JValue): Option[Address] = {
      jv match {
        case JNothing => None
        case JObject(_) => Some(apply(jv))
        case unexpected @ _ => sys.error("Unexpected JField type when deserializing Address: %s".format(unexpected))
      }
    }

    def apply(jv: JValue): Address = {
      new Address(
        address1 = (jv \ "address1").extract[String],
        address2 = (jv \ "address2").extract[String],
        city = (jv \ "city").extract[String],
        country = (jv \ "country").extract[String],
        phone = (jv \ "phone").extract[String],
        province = (jv \ "province").extract[String],
        postalCode = (jv \ "zip").extract[String],
        name = (jv \ "name").extract[String],
        countryCode = (jv \ "country_code").extract[String],
        provinceCode = (jv \ "province_code").extract[String]
      )
    }
  }
  
  class Address(
    val address1: String,
    val address2: String,
    val city: String,
    //company: String,
    val country: String,
    //firstName: String,
    //lastName: String,
    //val latitude: Double,
    //val longitude: Double,
    val phone: String,
    val province: String,
    val postalCode: String,
    val name: String,
    val countryCode: String,
    val provinceCode: String
  )
  
  
  class SkuLineItem(
    //fulfillment_service": "manual",
    val fulfillmentStatus: FulfillmentStatus,
    //grams": 200,
    val id: Long,
    val price: BigDecimal,
    val productId: Long,
    val quantity: Int,
    //requires_shipping": true,
    val sku: String,
    val title: String,
    val variantId: Long,
    val variantTitle: String,
    //vendor": null,
    val name: String,
    //variant_inventory_management": "shopify",
    //properties": [
    //      {
    //        "name": "Custom Engraving",
    //        "value": "Happy Birthday"
    //      }
    //    ],
    val productExists: Boolean
  )
  
  class Customer(
    val id: Long,
    val createdAt: DateTime,
    val email: String,
    val firstName: String,
    val lastName: String,
    val defaultAddress: Order.Address
  )
  
  object Customer {
    def apply(jv: JValue) = {
      new Customer(
        // "accepts_marketing": false,
        createdAt = new DateTime((jv \ "created_at").extract[String]),
        email = (jv \ "email").extract[String],
        firstName = (jv \ "first_name").extract[String],
        id = (jv \ "id").extract[Long],
        lastName = (jv \ "last_name").extract[String],
        //"last_order_id": null,
        //"multipass_identifier": null,
        //"note": null,
        //"orders_count": 0,
        //"state": "disabled",
        //"total_spent": "0.00",
        //"updated_at": "2013-08-21T18:26:09-04:00",
        //"verified_email": true,
        //"tags": "",
        //"last_order_name": null,
        //"image_url": "//gravatar.com/avatar/969daf6e3a99f7c72fd330af0b432435?default=http%3A%2F%2Fcdn.shopify.com%2Fs%2Fimages%2Fadmin%2Fcustomers%2Fcustomers_avatar_usa_whitehouse.png",
        defaultAddress = Address(jv \ "default_address")
      )
    }
  }
    
  def unapply(jsonString: String): Option[Order] = {
    None
  }

  private def extractLineItems(jv: JValue) = {
    jv match {
      case JArray(list) =>
        list.map(jvItem => {
          new SkuLineItem(
            //fulfillmentService
            fulfillmentStatus = FulfillmentStatus(jvItem \ "fulfillment_status"),
            //grams": 200,
            id = (jvItem \ "id").extract[Long],
            price = BigDecimal((jvItem \ "price").extract[String]),
            productId = (jvItem \ "product_id").extract[Long],
            quantity = (jvItem \ "quantity").extract[Int],
            //requires_shipping": true,
            sku = (jvItem \ "sku").extract[String],
            title = (jvItem \ "title").extract[String],
            variantId = (jvItem \ "variant_id").extract[Long],
            variantTitle = (jvItem \ "variant_title").extract[String],
            //vendor: null,
            name = (jvItem \ "name").extract[String],
            //variant_inventory_management": "shopify",
            /*
            properties": [
                  {
                    "name": "Custom Engraving",
                    "value": "Happy Birthday"
                  }
                ],
                */
            productExists = (jvItem \ "product_exists").extract[Boolean]
          )
        })
      case _ =>
        sys.error("Failed to parse line items")
    }
  }
  
  def apply(jv: JValue): Order = {
     //TODO: trace? println(jv.toString)
      
    
    new Order(
      confirmed = (jv \ "confirmed").extract[Boolean],
      createdAt = new DateTime((jv \ "created_at").extract[String]),
      currency = (jv \ "currency").extract[String],
      customer = Customer(jv \ "customer"),
      financialStatus = FinancialStatus(jv \ "financial_status"),
      fulfillmentStatus = FulfillmentStatus(jv \ "fulfillment_status"),
      gateway = (jv \ "gateway").extract[String],
      id = (jv \ "id").extract[Long],
      taxesIncluded = (jv \ "taxes_included").extract[Boolean],
      test = (jv \ "test").extract[Boolean],
      token = (jv \ "token").extract[String],
      totalPrice = BigDecimal((jv \ "total_price").extract[String]),
      totalTax = BigDecimal((jv \ "total_tax").extract[String]),
      orderNumber = (jv \ "order_number").extract[Long],
      lineItems = extractLineItems(jv \ "line_items"),
      shippingAddress = Address.getOpt(jv \ "shipping_address"),
      billingAddress = Address.getOpt(jv \ "billing_address")
    )
  }

  object List {
    def unapply(jsonString: String): Option[List[Order]] = {
      parseOpt(jsonString) match {
        case Some(JObject(JField("orders", JArray(orders)) :: Nil)) => Some(orders.map(apply))
        case _ => None // TODO: failed to parse
      }
    }
  }
}


class Order(/*
  "buyer_accepts_marketing": false,
  "cancel_reason": null,
  "cancelled_at": null,
  "cart_token": "68778783ad298f1c80c3bafcddeea02f",
  "checkout_token": null,
  "closed_at": null,
  */
  val confirmed: Boolean,
  val createdAt: DateTime,
  val currency: String,
  /*
  "email": "bob.norman@hostmail.com",
  */
  val financialStatus: Order.FinancialStatus,
  val fulfillmentStatus: Order.FulfillmentStatus,
  val gateway: String,
  val id: Long,
  /*
  "landing_site": "http://www.example.com?source=abc",
  "location_id": null,
  "name": "#1001",
  "note": null,
  "number": 1,
  "reference": "fhwdgads",
  "referring_site": "http://www.otherexample.com",
  "source": null,
  "subtotal_price": "398.00",
  */
  val taxesIncluded: Boolean,
  val test: Boolean,
  val token: String,
  /*
  "total_discounts": "0.00",
  "total_line_items_price": "398.00",
  */
  val totalPrice: BigDecimal,
  /*
  "total_price_usd": "409.94",
  */
  val totalTax: BigDecimal,
  /*
  "total_weight": 0,
  "updated_at": "2008-01-10T11:00:00-05:00",
  "user_id": null,
  "browser_ip": null,
  "landing_site_ref": "abc",
  */
  val orderNumber: Long
  /*
  "discount_codes": [
    {
      "code": "TENOFF",
      "amount": "10.00"
    }
  ],
  "note_attributes": [
    {
      "name": "custom engraving",
      "value": "Happy Birthday"
    },
    {
      "name": "colour",
      "value": "green"
    }
  ],
  "processing_method": "direct",
  "checkout_id": 450789469,
  */
  ,
  val lineItems: Seq[Order.SkuLineItem]
    /*
  "shipping_lines": [
    {
      "code": "Free Shipping",
      "price": "0.00",
      "source": "shopify",
      "title": "Free Shipping"
    }
  ],
  "tax_lines": [
    {
      "price": "11.94",
      "rate": 0.06,
      "title": "State Tax"
    }
  ],
  "payment_details": {
    "avs_result_code": null,
    "credit_card_bin": null,
    "cvv_result_code": null,
    "credit_card_number": "XXXX-XXXX-XXXX-4242",
    "credit_card_company": "Visa"
  }*/,
  val billingAddress: Option[Order.Address],
  val shippingAddress: Option[Order.Address]
  /*
  "fulfillments": [
    {
      "created_at": "2013-08-21T18:26:09-04:00",
      "id": 255858046,
      "order_id": 450789469,
      "service": "manual",
      "status": "failure",
      "tracking_company": null,
      "updated_at": "2013-08-21T18:26:09-04:00",
      "tracking_number": "1Z2345",
      "tracking_numbers": [
        "1Z2345"
      ],
      "tracking_url": "http://www.google.com/search?q=1Z2345",
      "tracking_urls": [
        "http://www.google.com/search?q=1Z2345"
      ],
      "receipt": {
        "testcase": true,
        "authorization": "123456"
      },
      "line_items": [
        {
          "fulfillment_service": "manual",
          "fulfillment_status": null,
          "grams": 200,
          "id": 466157049,
          "price": "199.00",
          "product_id": 632910392,
          "quantity": 1,
          "requires_shipping": true,
          "sku": "IPOD2008GREEN",
          "title": "IPod Nano - 8gb",
          "variant_id": 39072856,
          "variant_title": "green",
          "vendor": null,
          "name": "IPod Nano - 8gb - green",
          "variant_inventory_management": "shopify",
          "properties": [
            {
              "name": "Custom Engraving",
              "value": "Happy Birthday"
            }
          ],
          "product_exists": true
        }
      ]
    }
  ],
  "client_details": {
    "accept_language": null,
    "browser_ip": "0.0.0.0",
    "session_hash": null,
    "user_agent": null
  }*/,
  val customer: Order.Customer
) {
  def address = shippingAddress.orElse(billingAddress).getOrElse(customer.defaultAddress)
}

