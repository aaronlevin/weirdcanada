package org.weirdcanada.distro.data

import java.math.MathContext
import net.liftweb.mapper._
import org.joda.time.DateTime
import org.weirdcanada.distro.data._
import org.weirdcanada.distro.api.shopify.Order

class Sale extends LongKeyedMapper[Sale] with IdPK with Address {
  def getSingleton = Sale

  object orderId extends MappedLong(this) //with DBIndexed
  object lineItemId extends MappedLong(this)
  object dateTime extends MappedDateTime(this) with DBIndexed
  object consignedItem extends MappedLongForeignKey(this, ConsignedItem)
  object format extends MappedEnum(this, Album.Type)

  object amount extends MappedDecimal(this, MathContext.DECIMAL32, 2)
  object quantity extends MappedInt(this)
  object markUp extends MappedDecimal(this, MathContext.DECIMAL32, 2)
  object paidToConsignor extends MappedDecimal(this, MathContext.DECIMAL32, 2)
  
  object sku extends MappedString(this, 32)
  object customerId extends MappedLong(this) // Comes from Shopify
  
  object consignor extends MappedLongForeignKey(this, Account)
}

// The companion object to the above Class
object Sale extends Sale with LongKeyedMetaMapper[Sale] {
  override def dbIndexes = UniqueIndex(orderId) :: super.dbIndexes
  
  def getLatestOrderId = {
    DB.runQuery("SELECT MAX(orderId) FROM sale")._2.headOption.flatMap(_.headOption).filterNot(_ == null).map(_.toLong)
  }

  /**
   * Get all sales for an account by an account id.
   *
   * @param accountId the id of the account
   * @returns a list of `Sale`
   */
  def getSalesByAccount(accountId: Long): List[Sale] = {
    Sale.findAll(By(consignor, accountId))
  }

  /**
   * Helper method to filter sales by a start and end date
   */
  def filter(sales: List[Sale], format: String, startDate: DateTime, endDate: DateTime): List[Sale] = 
    sales
      .filter { s =>
        s.dateTime.is.before(endDate.toDate) && 
        !s.dateTime.is.before(startDate.toDate) && {
          if( format == "all" )
            true
          else
            s.consignedItem.obj.flatMap { _.album.obj.map { _.isOfType(format) }}.openOr (false)
        }
      }
  
  def findByOrderId(orderId: Long) = {
    this.find(By(Sale.orderId, orderId))
  }
}
