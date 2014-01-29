package org.weirdcanada.distro.data

import net.liftweb.mapper._
import net.liftweb.util._
import java.math.MathContext
import scala.collection.mutable.ListBuffer
  
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
  
  object role extends MappedEnum(this, UserRole)
  
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
object Account extends Account with LongKeyedMetaMapper[Account] {
  override def dbIndexes = UniqueIndex(email) :: super.dbIndexes
  
  def findByEmailAddress(emailAddress: String) =
    Account.find(By(Account.email, emailAddress))
  
  // This is used for cookie logins
  def findByWcdId(wcdid: String) =
    Account.find(By(Account.wcdid, wcdid))

  def findByPartialName(name: String) = Account.findAllByPreparedStatement({ conn => 
    val stmt = conn.connection.prepareStatement("""select firstname, lastname, organization from account where lower(firstname) like lower(?) OR lower(lastname) like lower(?) OR lower(organization) like lower(?) """)

    stmt.setString(1,"%" + name + "%")
    stmt.setString(2,"%" + name + "%")
    stmt.setString(3,"%" + name + "%")
    stmt
  })
}
