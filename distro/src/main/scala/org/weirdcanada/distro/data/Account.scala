package org.weirdcanada.distro.data

import net.liftweb.mapper._
import net.liftweb.util._
import org.joda.time.DateTime
import org.weirdcanada.common.util.{StringParsingUtil, StringUtils}
import StringParsingUtil.safeParse
import org.weirdcanada.dynamicform.{BasicField, DynamicField, DynamicFormFieldRenderHelpers, HasFields, HasEmpty, ManyRecordField, ManyTypeaheadField, S3Image, S3Resource}
import java.math.MathContext
import java.util.Date
import scala.collection.mutable.ListBuffer
import scalaz.Lens
import scalaz.\/
import scalaz.{\/-,-\/} // Zoidberg


object UserRole extends Enumeration {
  type Roles = Value
  val Visitor =  Value(0)
  val UnconfirmedMember = Value(1)
  val Member = Value(2)
  val Intern = Value(3)
  val Admin = Value(4)
  // TODO: any values to add
}

class Account extends LongKeyedMapper[Account] with IdPK with OneToMany[Long, Account] with Address {
  def getSingleton = Account

  object wcdid extends MappedString(this, 32) with DBIndexed
  object firstName extends MappedString(this, 64)
  object lastName extends MappedString(this, 64)
  object email extends MappedString(this, 128) //with DBIndexed
  object password extends MappedPassword(this)
  object emailConfirmationKey extends MappedString(this, 16) with DBIndexed
  object emailValidated extends MappedBoolean(this) // True if we've verified their primary address

  object paypalEmail extends MappedString(this, 128)
  object organization extends MappedString(this, 256)
  object phoneNumber extends MappedString(this, 20)
  object notes extends MappedText(this)

  object unofficialBalance extends MappedDecimal(this, MathContext.DECIMAL32, 2)

  object consignedItems extends MappedOneToMany(ConsignedItem, ConsignedItem.consignor, OrderBy(ConsignedItem.createdAt, Ascending))

  object sales extends MappedOneToMany(Sale, Sale.consignor)
  object payments extends MappedOneToMany(Payment, Payment.consignor)

  object role extends MappedEnum(this, UserRole)

  object payments extends MappedOneToMany(Payment, Payment.consignor, OrderBy(Payment.requestedAt, Descending))

  def displayName =
    List(firstName.is, lastName.is).filterNot(_.isEmpty).mkString(" ") match { case "" => "[No Name]" case name => name }

  override def toString =
    "Account(id=%s, name=%s, email=%s, role=%s, city=%s, province=%s, balance=%s)"
      .format(
        id.is,
        displayName,
        email.is,
        role.is.toString,
        city.is,
        province.is,
        unofficialBalance.is
      )
}


/**
 * The singleton that has methods for accessing the database
 */
object Account
  extends Account
  with LongKeyedMetaMapper[Account]
  with MapperObjectUtils[Account] {

  override def dbIndexes = UniqueIndex(email) :: super.dbIndexes

  def findByEmailAddress(emailAddress: String) =
    Account.find(By(Account.email, emailAddress))

  // This is used for cookie logins
  def findByWcdId(wcdid: String) =
    Account.find(By(Account.wcdid, wcdid))

  def amountOwed(account: Account): BigDecimal = {
    val sales = account.sales.foldLeft(BigDecimal(0.0)){ (acc, sale) => (sale.amount.is - sale.markUp.is) }
    val payments = 
      account
        .payments
        .filterNot { payment => Option(payment.paidAt.is).isEmpty }
        .foldLeft(BigDecimal(0.0)) { (acc, payment) => acc + payment.amount.is }
    (sales - payments)
  }

  def findByPartialName(name: String) = Account.findAllByPreparedStatement({ conn => 
    val stmt = conn.connection.prepareStatement("""select id, firstname, lastname, organization from account where lower(firstname) like lower(?) OR lower(lastname) like lower(?) OR lower(organization) like lower(?) """)

    stmt.setString(1,"%" + name + "%")
    stmt.setString(2,"%" + name + "%")
    stmt.setString(3,"%" + name + "%")
    stmt
  })

  import Payment.PaymentData

  case class AccountData(
    id: Option[Long],
    firstName: String,
    lastName: String,
    email: String,
    paypalEmail: String,
    organization: String,
    phoneNumber: String,
    payments: Map[Int, PaymentData]
  )

  private val accountIdLens: Lens[AccountData, String] = Lens.lensu(
    (a,i) => a.copy(id = safeParse[Long](i)),
    (a) => a.id.map { _.toString }.getOrElse { "" }
  )
  private val firstNameLens: Lens[AccountData, String] = Lens.lensu(
    (a,fn) => a.copy(firstName = fn),
    (a) => a.firstName
  )
  private val lastNameLens: Lens[AccountData, String] = Lens.lensu(
    (a,ln) => a.copy(lastName = ln),
    (a) => a.lastName
  )
  private val emailLens: Lens[AccountData, String] = Lens.lensu(
    (a,e) => a.copy(email = e),
    (a) => a.email
  )
  private val paypalEmailLens: Lens[AccountData, String] = Lens.lensu(
    (a,e) => a.copy(paypalEmail = e),
    (a) => a.paypalEmail
  )
  private val organizationLens: Lens[AccountData, String] = Lens.lensu(
    (a,o) => a.copy(organization = o),
    (a) => a.organization
  )
  private val phoneLens: Lens[AccountData, String] = Lens.lensu(
    (a,p) => a.copy(phoneNumber = p),
    (a) => a.phoneNumber
  )
  private val paymentsLens: Lens[AccountData, Map[Int, PaymentData]] = Lens.lensu(
    (a,mp) => a.copy(payments = mp),
    (a) => a.payments
  )

  implicit object accountDataFields extends HasFields[AccountData] {
    val fields: List[DynamicField[AccountData]] = List(
      BasicField[AccountData]("account-id", accountIdLens),
      BasicField[AccountData]("account-first-name", firstNameLens),
      BasicField[AccountData]("account-last-name", lastNameLens),
      BasicField[AccountData]("account-email", emailLens),
      BasicField[AccountData]("account-paypal-email", paypalEmailLens),
      BasicField[AccountData]("account-organization", organizationLens),
      BasicField[AccountData]("account-phone-number", phoneLens),
      ManyRecordField[AccountData, PaymentData]("account-payment", paymentsLens) 
    )
  }

  implicit object accountEmpty extends HasEmpty[AccountData] {
    val empty = AccountData(
      id = None,
      firstName = "",
      lastName = "",
      email = "",
      paypalEmail = "",
      organization = "",
      phoneNumber = "",
      payments = Map.empty[Int, PaymentData]
    )
  }

  /**
   * a bunch of methods that should be abstracted out but i'm too lazy
   */
  private def setAccountParamsFromData(data: AccountData, account: Account): \/[String, Account] = {
    try {
      account
        .firstName(data.firstName)
        .lastName(data.lastName)
        .email(data.email)
        .paypalEmail(data.paypalEmail)
        .organization(data.organization)
        .phoneNumber(data.phoneNumber)

      \/-(account)
    } catch {
      case e: Throwable => -\/("Something terrible happened for data: %s".format(data))
    }
  }

  def toData(account: Account): AccountData = AccountData(
    id = Some(account.id.is),
    firstName = account.firstName.is,
    lastName = account.lastName.is,
    email = account.email.is,
    paypalEmail = account.paypalEmail.is,
    organization = account.organization.is,
    phoneNumber = account.phoneNumber.is,
    payments = account.payments.map { Payment.toData }.zipWithIndex.map { _.swap }.toMap
  )

  def updateFromData(data: AccountData, account: Account): \/[String, Account] = 
    DB.use(DefaultConnectionIdentifier) { connection =>

      setAccountParamsFromData(data, account).map { account =>

        val currentPayments: Set[PaymentData] = account.payments.map { Payment.toData }.toSet
        val dataPayments: Set[PaymentData] = data.payments.values.toSet

        val removeablePayments = (currentPayments &~ dataPayments)
        val removeablePaymentIds = removeablePayments.flatMap { _.id }

        val newPayments = data.payments.values.filter { p =>
          !currentPayments.contains( p )
        }
        val deletePayments = account.payments.filter { p =>
          removeablePaymentIds.contains( p.id.is )
        }

        /**
         * update all payments that won't be deleted
         */
        (currentPayments & dataPayments).foreach { paymentData =>
          for {
            id <- paymentData.id
            payment <- Payment.findById(id)
          } yield Payment.updateFromData(paymentData, payment)(Some(account))
        }

        newPayments.foreach { Payment.fromData(_)(account) }
        deletePayments.foreach { account.payments -= _ }
        deletePayments.foreach { _.delete_! }

        account.saveMe
      }
    }

}
