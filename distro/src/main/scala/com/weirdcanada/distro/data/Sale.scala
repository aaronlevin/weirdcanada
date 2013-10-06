package com.weirdcanada.distro.data

import net.liftweb.mapper._
import com.weirdcanada.distro.data._
import java.math.MathContext
import com.weirdcanada.distro.api.shopify.Order

class Sale extends LongKeyedMapper[Sale] with IdPK with Address {
  def getSingleton = Sale

  object orderId extends MappedLong(this) with DBIndexed
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
  def getLatestOrderId = {
    DB.runQuery("SELECT MAX(orderId) FROM sale")._2.headOption.flatMap(_.headOption).filterNot(_ == null).map(_.toLong)
  }
}
