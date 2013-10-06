package com.weirdcanada.distro.job

import com.weirdcanada.distro.Config
import com.weirdcanada.distro.api.shopify.Shopify
import com.weirdcanada.distro.data.{Sale, ConsignedItem, Account}

/**
 * ShopifyClient does the work of connecting to Shopify and downloading new orders
 * 
 * For now, arbitrarily keep the last 50 status messages from the service
 */
class ShopifyClient(config: Config) extends IntervalService(config.shopifyPollingInterval * 1000, 50) {
  val shopify = new Shopify(config)
  
  override protected def fn {
    val options = Sale.getLatestOrderId.map("since_id" -> _).toArray
    
    val orders = shopify.getOrders(options : _*)
    val uniqueVariantIds = orders.flatMap(_.lineItems.map(_.variantId)).toSet
    val metafields = uniqueVariantIds.map(id => id -> shopify.getVariantMetafields(id, "namespace" -> "weirdcanada")).toMap // TODO: could cache locally in our DB...
    
    orders.foreach( order => {
      order.lineItems.foreach( lineItem => {
        for {
          metafields <- metafields.get(lineItem.variantId) orElse sys.error("Order #%s: Can't find metafields for variant: %s".format(order.id, lineItem.variantId))
          consignedItemGuid <- metafields.find(_.key == "guid").map(_.value) orElse sys.error("Order: #%s: guid missing for variant: %s".format(order.id, lineItem.variantId))
          consignedItem <- ConsignedItem.findByGuid(consignedItemGuid) orElse sys.error("Order #%s: Can't find item by guid: %s".format(order.id, consignedItemGuid))
          consignor <- consignedItem.consignor.obj orElse sys.error("Order #%s: Can't find consignor for item: %s".format(order.id, consignedItem.id.is))
        }
        yield {
          val weirdCanadaRevenue = lineItem.quantity * consignedItem.markUp.is
          val consignorRevenue = lineItem.price - weirdCanadaRevenue
          // Note: taxes appear as separate line items

          require(lineItem.quantity > 0, "Order #%s: Found invalid quantity on line item #%s".format(order.id, lineItem.id))
          require(weirdCanadaRevenue >= 0, "Order #%s: Found negative WC revenue on line item #%s".format(order.id, lineItem.id))
          require(consignorRevenue >= 0, "Order #%s: Found negative consignor revenue on line item #%s".format(order.id, lineItem.id))
          
          // Create a record of the sale
          Sale
            .addressLine1(order.address.address1)
            .addressLine2(order.address.address2)
            .city(order.address.city)
            .province(order.address.province)
            .postalCode(order.address.postalCode)
            .country(order.address.country)
            .orderId(order.id)
            .dateTime(order.createdAt.toDate)
            .lineItemId(lineItem.id)
            .sku(lineItem.sku)
            .amount(lineItem.price)
            .customerId(order.customer.id)
            .consignedItem(consignedItem)
            .consignor(consignor)
            .markUp(weirdCanadaRevenue)
            .paidToConsignor(0)
            .save

          // TODO: perhaps report unofficial WC revenue to stats manager?
          
          // Update the account of the consignor
          consignor
            .unofficialBalance(consignor.unofficialBalance.is + consignorRevenue)
            .save
            
          // TODO: deplete quantity of the consigned item? (not currently tracking quantity on the ConsignedItem table)
            
          addMessage("success", "Order #%s: Processed transaction %s, sku %s for $%,0.2f".format(order.id, lineItem.id, lineItem.sku, lineItem.price))
        }
      }) // lineItems
    }) // orders
  }
}

object ShopifyClient {
  def apply(config: Config) = new ShopifyClient(config)
}