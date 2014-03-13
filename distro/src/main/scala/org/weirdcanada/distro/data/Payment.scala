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

object Payment extends Payment with LongKeyedMetaMapper[Payment] {
  // The account has a payment pending if there is a record with the account but no paidAt date.
  // Note: to handle rejected payment claims, should set the paidAt date to current date and the amount to 0
  // Alternately, could add a payment status (or resolution) field that says what happened to the request.
  def hasPaymentPending(account: Account) =
    find(By(consignor, account), NullRef(paidAt)).isDefined
}
