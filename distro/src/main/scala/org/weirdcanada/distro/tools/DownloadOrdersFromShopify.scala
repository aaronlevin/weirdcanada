package org.weirdcanada.distro.tools

import net.liftweb.json._
import net.liftweb.common.Loggable
import org.weirdcanada.distro.Config
import org.weirdcanada.distro.api.shopify.{Shopify, Metafield, Product}
import org.weirdcanada.distro.service.DatabaseManager
import org.weirdcanada.distro.util.NullEmailFactory
import org.weirdcanada.distro.data.{Sale, ConsignedItem}
import scala.io.Source
import net.liftweb.common.Full
import org.weirdcanada.distro.api.shopify.Variant
import org.weirdcanada.distro.util.AnyExtensions._
import scala.annotation.tailrec
import org.weirdcanada.distro.util.IdList
import org.weirdcanada.distro.util.Parse._

object DownloadOrdersFromShopify extends App with Loggable {
  val config = Config.fromLiftProps
  val dbManager = new DatabaseManager(config)
  val shopify = new Shopify(config)

  dbManager.connect
  //dbManager.createSchema

  case class Args(
    limit: Int = 50,
    id: Option[Long] = None,
    since: Option[Long] = None,
    verbose: Boolean = false,
    writeToDb: Boolean = false,
    status: Option[String] = None,
    financialStatus: Option[String] = None,
    fulfillmentStatus: Option[String] = None,
    forceGuid: Option[String] = None,
    ignoreErrors: Boolean = false
  )
  
  object Args {
    private object Default extends Args()
          
    def apply() = parseArgs(args.toList, Default)

    @tailrec
    private def parseArgs(remainingArgs: List[String], args: Args): Args = {
      remainingArgs match {
        case Nil =>
          args
        
        case "-v" :: tail =>
          parseArgs(tail, args.copy(verbose = true))

        // not implemented yet
        //case "-id" :: id :: tail =>
        //  parseArgs(tail, args.copy(id = Some(id.toLong)))
        
        case "-since" :: SafeLong(id) :: tail =>
          parseArgs(tail, args.copy(since = Some(id.toLong)))
          
        case "-since" :: tail =>
          parseArgs(tail, args.copy(since = Sale.getLatestOrderId))
          
        case "-save" :: tail =>
          parseArgs(tail, args.copy(writeToDb = true))
          
        case "-n" :: SafeInt(limit) :: tail =>
          parseArgs(tail, args.copy(limit = limit))
          
        case "-status" :: status :: tail =>
          parseArgs(tail, args.copy(status = Some(status)))
          
        case "-fins" :: status :: tail =>
          parseArgs(tail, args.copy(financialStatus = Some(status)))
          
        case "-fuls" :: status :: tail =>
          parseArgs(tail, args.copy(fulfillmentStatus = Some(status)))
          
        case "-force-guid" :: guid :: tail =>
          parseArgs(tail, args.copy(forceGuid = Some(guid)))
          
        case "-ignore-errors" :: tail =>
          parseArgs(tail, args.copy(ignoreErrors = true))
          
        case unexpectedToken :: tail =>
          println("Unexpected token: " + unexpectedToken)
          sys.exit(1)
      }
    }
  }
  
  (new DownloadOrdersFromShopify(Args(), shopify)).download
}
import DownloadOrdersFromShopify.Args

class DownloadOrdersFromShopify(args: Args, shopify: Shopify) {
  // Helper method that only prints a message when the verbose flag is set
  private def vprintln(msg: => String) {
    if (args.verbose)
      println(msg)
  }
  
  // Helper method to display an error message and exit (or continue if the user chose to ignore errors)
  private def exit[T](msg: => String): Option[T] = {
    println(msg)
    if (!args.ignoreErrors)
      sys.exit(1)
    None
  }
  
  def download = {
    vprintln("Using " + args.toString)
    
    val options =
      List(
        args.since.map("since_id" -> _),
        args.status.map("status" -> _),
        args.financialStatus.map("financial_status" -> _),
        args.fulfillmentStatus.map("fulfillment_status" -> _)
      ).flatten.toArray
    

    vprintln("downloading orders...")
    val orders = shopify.getOrders(options : _*)
    
    
    // Helper method to make the for-comprehension below a little cleaner
    def require(bool: => Boolean) = if (bool) Some(true) else None
    
    
    // Iterate through the orders, processing one at a time
    orders.foreach( order => {
      vprintln("processing order %d".format(order.id))

      // Process each SKU line item on the order, one at a time
      order.lineItems.foreach( lineItem => {
        try {
          vprintln("found sku: " + lineItem.sku)
          
          for {
            consignedItem <- ConsignedItem.findBySku(lineItem.sku) orElse exit("can't find item by sku: %s".format(lineItem.sku))
            consignor <- consignedItem.consignor.obj orElse exit("can't find consignor for item: %s".format(consignedItem.id.is))
            weirdCanadaRevenue = lineItem.quantity * consignedItem.markUp.is
            consignorRevenue = lineItem.price - weirdCanadaRevenue
            _ <- require(lineItem.quantity > 0)   orElse exit("invalid quantity: %d".format(lineItem.quantity))
            _ <- require(weirdCanadaRevenue >= 0) orElse exit("negative WC revenue: $%s".format(weirdCanadaRevenue))
            _ <- require(consignorRevenue >= 0)   orElse exit("negative consignor revenue: $%s".format(consignorRevenue))
          }
          yield {
            // Note: taxes appear as separate line items
            
            if (args.writeToDb) {
              val (sale, isNew) = 
                Sale.findByOrderId(order.id) // Does this order already exist in the database?
                  .map(_ -> false)
                  .getOrElse(Sale.create -> true) // Create a new record of the sale

              sale
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

              // Update the account of the consignor (but only if this sale is new to the system)
              if (sale.save && isNew) {
                consignor
                  .unofficialBalance(consignor.unofficialBalance.is + consignorRevenue)
                  .save
              }
            }

            println("order %s, lineitem %s: sku %s @ $%,0.2f x %d for $%,0.2f".format(order.id, lineItem.id, lineItem.sku, consignedItem.customerCost.is, lineItem.quantity, lineItem.price))
          } // yield
        }
        catch {
          case e: Exception =>
            if (args.verbose) e.printStackTrace
            exit(e.toString)
        }
      }) // lineItems
    }) // orders
  } // def download
}
