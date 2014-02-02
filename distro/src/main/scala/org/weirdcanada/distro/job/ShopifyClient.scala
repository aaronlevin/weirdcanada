package org.weirdcanada.distro.job

import org.weirdcanada.distro.Config
import org.weirdcanada.distro.api.shopify.Shopify
import org.weirdcanada.distro.data.{Sale, ConsignedItem, Account}

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
    //val uniqueVariantIds = orders.flatMap(_.lineItems.map(_.variantId)).toSet
    
    orders.foreach( order => {
      order.lineItems.foreach( lineItem => {
        for {
          consignedItem <- ConsignedItem.findBySku(lineItem.sku) orElse sys.error("Order #%s: Can't find item by sku: %s".format(order.id, lineItem.sku))
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