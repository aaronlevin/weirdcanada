package org.weirdcanada.distro.data

import java.math.MathContext
import net.liftweb.mapper._
import org.joda.time.DateTime
import org.weirdcanada.dynamicform.{BasicField, DynamicField, DynamicFormFieldRenderHelpers, HasFields, HasEmpty, ManyRecordField, ManyTypeaheadField, TypeaheadField}
import org.weirdcanada.common.util.{StringParsingUtil}
import StringParsingUtil.safeParse
import scalaz.Lens
import scalaz.\/
import scalaz.{\/-,-\/} // Zoidberg

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

  def findById(paymentId: Long) = 
    find(By(id,paymentId)) 
  /**
  def payments(account: Account) = {
    val results = find(By(consignor, account))
    val total = results.foldLeft(0){ _ + _.amount.is }
  }**/


  /**
   * Payment ADT
   */
  case class PaymentData(
    id: Option[Long],
    requestedAt: DateTime,
    paidAt: Option[DateTime],
    amount: BigDecimal,
    consignorId: Option[Long],
    notes: String
  )

  /**
   * payment data lenses
   */
  private val paymentIdLens: Lens[PaymentData, String] = Lens.lensu(
    (p,i) => p.copy(id = safeParse[Long](i)),
    (p) => p.id.map { _.toString }.getOrElse { "" }
  )
  private val requestedAtLens: Lens[PaymentData, String] = Lens.lensu(
    (p,ds) => p.copy(requestedAt = new DateTime(ds)),
    (p) => p.requestedAt.toString("YYYY-MM-dd")
  )
  private val paidAtLens: Lens[PaymentData, String] = Lens.lensu(
    (p,ps) => {println("xxx string: %s".format(ps)); p.copy(paidAt = Some(new DateTime(ps))) },
    (p) => p.paidAt.map { _.toString("YYYY-MM-dd") }.getOrElse { "" }
  )
  private val amountLens: Lens[PaymentData, String] = Lens.lensu(
    (p,a) => safeParse[BigDecimal](a).map { amnt => p.copy(amount = amnt) }.getOrElse { p },
    (p) => p.amount.toString
  )
  private val consignorIdLens: Lens[PaymentData, String] = Lens.lensu(
    (p,cid) => p.copy(consignorId = safeParse[Long](cid)),
    (p) => p.consignorId.map { _.toString }.getOrElse { "" }
  )
  private val notesLens: Lens[PaymentData, String] = Lens.lensu(
    (p,n) => {println("xxx notes: %s".format(n)); p.copy(notes = n) },
    (p) => p.notes
  )

  /**
   * Select fields
   */
  import DynamicFormFieldRenderHelpers.{datePickerRender}

  private val requestedDateSelect = 
    datePickerRender(requestedAtLens.get)("@payment-requested-at-input")("yyyy-mm-dd") _
  private val paidSelect = 
    datePickerRender(paidAtLens.get)("@payment-paid-at-input")("yyyy-mm-dd") _

  /**
   * Witness to the `HasFields` typeclass
   */
  implicit object PaymentDataFields extends HasFields[PaymentData] {
    val fields: List[DynamicField[PaymentData]] = List(
      BasicField[PaymentData]("payment-requested-at", requestedAtLens, Some(requestedDateSelect)),
      BasicField[PaymentData]("payment-paid-at", paidAtLens, Some(paidSelect)),
      BasicField[PaymentData]("payment-amount", amountLens),
      BasicField[PaymentData]("payment-notes", notesLens)
    )
  }

  implicit object PaymentDataEmpty extends HasEmpty[PaymentData] {
    val empty = PaymentData(
      id = None,
      requestedAt = new DateTime,
      paidAt = None,
      amount = 0.0,
      consignorId = None,
      notes = ""
    )
  }

  private def setPaymentParamsFromData(data: PaymentData, payment: Payment, consignor: Option[Account]): \/[String, Payment] = 
    try {
      payment
        .requestedAt(data.requestedAt.toDate)
        .amount(data.amount)
        .notes(data.notes)

      data.paidAt.foreach { p => payment.paidAt(p.toDate) }
      consignor.foreach { c => payment.consignor(c) }

      \/-(payment)
    } catch {
      case e: Throwable => -\/("Something terrible happened for data: %s".format(data))
    }

  def toData(payment: Payment): PaymentData = PaymentData(
    id = Some(payment.id.is),
    requestedAt = new DateTime(payment.requestedAt.is),
    paidAt = Option(payment.paidAt.is).map { d => new DateTime(d) },
    amount = payment.amount.is,
    consignorId = Some(payment.consignor.is),
    notes = payment.notes.is
  )

  /**
   * TODO: don't squash error here
   */
  def fromData(data: PaymentData)(account: Account): Option[Payment] = 
    setPaymentParamsFromData(data, Payment.create, Some(account)) match {
      case \/-(payment) => Some(payment.saveMe)
      case -\/(error) => None
  }

  def updateFromData(data: PaymentData, payment: Payment)(account: Option[Account]): \/[String, Payment] = 
    setPaymentParamsFromData(data, payment, account).map { _.saveMe }

}
