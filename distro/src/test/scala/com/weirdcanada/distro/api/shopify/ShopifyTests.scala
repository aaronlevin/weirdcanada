package com.weirdcanada.distro.api.shopify

import com.weirdcanada.distro.Config
import com.weirdcanada.distro.api.shopify

import org.specs2.mutable.Specification
import org.specs2.specification.AroundExample
import org.specs2.execute.AsResult

object ShopifyTests extends Specification {
  "Shopify.getOrders" should {
    val config = Config.fromLiftProps
    val shopify = new Shopify(config)

    "Respect limits" in {
      val orders = shopify.getOrders("limit" -> 2)

      orders.length must be_==(2)
    }
    
    "Respect since_id" in {
      val firstOrder :: secondOrder :: Nil = shopify.getOrders("limit" -> 2)
      
      val afterFirstOrder = shopify.getOrders("limit" -> 1, "since_id" -> firstOrder.id).head
      
      firstOrder.id must be_!=(secondOrder.id)
      secondOrder.id must be_==(afterFirstOrder.id)
    }
  }
}