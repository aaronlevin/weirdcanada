package org.weirdcanada.distro.data

import net.liftweb.mapper._
import java.math.MathContext

class Payment extends LongKeyedMapper[Payment] with IdPK {
  def getSingleton = Payment

  object requestedAt extends MappedDateTime(this) with DBIndexed
  object paidAt extends MappedDateTime(this)
  object amount extends MappedDecimal(this, MathContext.DECIMAL32, 2)
  object consignor extends MappedLongForeignKey(this, Account)
  object notes extends MappedText(this)
}

// The companion object to the above Class
object Payment extends Payment with LongKeyedMetaMapper[Payment] {
  
}
