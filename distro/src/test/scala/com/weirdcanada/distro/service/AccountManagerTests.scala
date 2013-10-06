package org.weirdcanada.distro.service

import scala.xml.NodeSeq
import org.specs2.mutable.Specification
import org.specs2.specification.{BeforeExample, AroundExample}
import org.specs2.execute.{AsResult, Result}
import org.specs2.mock._
import org.mockito.Matchers.{eq => _eq, _} // AnyRef.eq messes up the eq Mockito matcher
import net.liftweb.mapper._
import net.liftweb.http.{S, LiftSession}
import net.liftweb.util.Helpers.randomString
import net.liftweb.util.Helpers.tryo
import net.liftweb.common.Empty
import org.weirdcanada.distro.util.{EmailFactory, NullEmailFactory}
import org.weirdcanada.distro.data.Account
import org.weirdcanada.distro.Config
import org.weirdcanada.distro.api.shopify

object AccountManager_createAccountTests extends Specification with AroundExample with Mockito {
  val config = Config.fromLiftProps
  val service = new Service(config, NullEmailFactory)
  val validEmail = "test-user@weirdcanada.com"
  val invalidEmail = "Not an email address"
  val validPassword = "password"
  val tooShortPassword = "1234567"
  val firstName = "Foo"
  val lastName = "Bar"
  val validAddress1 = "123 Main St"
  val address2 = "Unit #1"
  val city = "Waterloo"
  val province = "ON"
  val validPostalCode = "N1N2N3"
  val country = "Canada"
  val validPhoneNumber = "519-555-1234"
  val validPaypalEmail = "1234567890@paypal.com"

  // Run these tests in a transaction so we don't get interference from parallel Specifications
  def around[T: AsResult](t: => T): Result = {
    DB.use(DefaultConnectionIdentifier) { cn => {
      Account.bulkDelete_!!()
      AsResult(t)
    }}
  }

  
  "AccountManager.createAccount" should {
    tryo {
      service.DatabaseManager.connect
      service.DatabaseManager.createSchema
    }

  
    // TODO: reject invalid email addresses
    // TODO: reject invalid passwords
    // TODO: reject all other invalid/missing fields

    "Doesn't send confirmation when registration fails" in {
      val mockEmailFactory = mock[EmailFactory]
      val service = new Service(config, mockEmailFactory)

      service.AccountManager.createAccount(validEmail, "", "")
   
      there was no (mockEmailFactory).send(any[String], any[String], any[String], any[NodeSeq])
    }

    "Reject duplicate email addresses" in {
      val accountCountBefore = Account.count
      val accountBox1 = service.AccountManager.createAccount(validEmail, validPassword, firstName, lastName, validAddress1, address2, city, province, validPostalCode, country, validPhoneNumber, validPaypalEmail)
      val accountBox2 = service.AccountManager.createAccount(validEmail, "blah blah", "another", "user", validAddress1, address2, city, province, validPostalCode, country, validPhoneNumber, "foo@paypal.com")
      val accountCountAfter = Account.count

      accountCountAfter must be equalTo(accountCountBefore + 1)
      accountBox1.toOption must beSome
      accountBox2.toOption must beNone
    }
    
    "Accept valid data and set all fields" in {
      val accountBox = service.AccountManager.createAccount(validEmail, validPassword, firstName, lastName, validAddress1, address2, city, province, validPostalCode, country, validPhoneNumber, validPaypalEmail)
      
      accountBox.toOption must beSome
      val account = accountBox.toOption.get

      account.email.is must be(validEmail)
      account.password.match_?(validPassword) must beTrue
      account.firstName.is must be(firstName)
      account.lastName.is must be(lastName)
      account.addressLine1.is must be(validAddress1)
      account.addressLine2.is must be(address2)
      account.city.is must be(city)
      account.province.is must be(province)
      account.postalCode.is must be(validPostalCode)
      account.country.is must be(country)
      account.phoneNumber.is must be(validPhoneNumber)
      account.paypalEmail.is must be(validPaypalEmail)
    }
    
    "Sends confirmation registration to user" in {
      val mockEmailFactory = mock[EmailFactory]
      val service = new Service(config, mockEmailFactory)

      service.AccountManager.createAccount(validEmail, validPassword, firstName, lastName, validAddress1, address2, city, province, validPostalCode, country, validPhoneNumber, validPaypalEmail)
   
      there was one(mockEmailFactory).send(_eq(validEmail), _eq(config.smtpUsername), any[String], any[NodeSeq])
    }
  }
}



object AccountManager_isValidEmailAddressTests extends Specification {
  "AccountManager.isValidEmailAddress" should {
    val config = Config.fromLiftProps
    val accountManager = new AccountManager(config, null)
    
    "Reject invalid addresses" in {
      accountManager.isValidEmailAddress("foo") must beFalse
      accountManager.isValidEmailAddress("foo@bar") must beFalse
    }
    
    "Accept valid emails" in {
      accountManager.isValidEmailAddress("foo@bar.com") must beTrue
      accountManager.isValidEmailAddress("foo.bar.baz@gmail.com") must beTrue
    }
  }
}


object AccountManager_confirmRegistrationTests extends Specification with AroundExample {
  val config = Config.fromLiftProps
  val service = new Service(config, NullEmailFactory)
  val validEmail = "exists@fake-domain-for-testing.com"
  val validKey = "VALID-KEY"
  def createAccount = service.AccountManager.createAccount(validEmail, "password", "Test").openOrThrowException("").emailConfirmationKey(validKey).saveMe
  def reset = Account.bulkDelete_!!()

  // Run these tests in a transaction so we don't get interference from parallel Specifications
  def around[T: AsResult](t: => T): Result = {
    DB.use(DefaultConnectionIdentifier) { cn => {
      reset
      createAccount
      AsResult(t)
    }}
  }
  
  
  "AccountManager.confirmRegistration" should {
    service.DatabaseManager.connect
    service.DatabaseManager.createSchema

    
    "Reject bogus email addresses" in {
      service.AccountManager.confirmRegistration("not-in-the-database@fake-domain-for-testing.com", "ABCD") must beEmpty
    }
    
    "Reject bogus confirm key" in {
      service.AccountManager.confirmRegistration(validEmail, "ABCD") must beEmpty
      
      val account = Account.findByEmailAddress(validEmail).toOption.get
      account.emailValidated.is must beFalse
    }
    
    "Accept valid email and confirm key" in {
      val accountOpt = service.AccountManager.confirmRegistration(validEmail, validKey).toOption
      
      accountOpt must beSome
      val account = accountOpt.get
      
      account.email.is must be equalTo(validEmail)
      account.emailValidated.is must beTrue
    }    
    
    "Reject empty confirm key" in {
      service.AccountManager.confirmRegistration(validEmail, "") must beEmpty // Must fail with unvalidated account
      service.AccountManager.confirmRegistration(validEmail, validKey).toOption
      service.AccountManager.confirmRegistration(validEmail, "") must beEmpty // Must fail with validated account
    }
  }
}


object AccountManager_canRequestPaymentTests extends Specification {
  "AccountManager.canRequestPayment" should {
    val config = Config.fromLiftProps
    val accountManager = new AccountManager(config, null)
    val validBalance = 50
    val invalidBalance = validBalance - 0.01
    
    "Deny non-validated accounts" in {
      val account = Account.create.email("foo@bar").emailValidated(false).unofficialBalance(validBalance)
      val response = accountManager.canRequestPayment(account)
      
      response must be(EmailNotValidated)
    }
    
    "Deny accounts with inadequate balance" in {
      val account = Account.create.email("foo@bar").emailValidated(true).unofficialBalance(invalidBalance)
      val response = accountManager.canRequestPayment(account)
      
      response must be(BalanceTooLow)
    }
    
    "Approve validated accounts with adequate balance" in {
      val account = Account.create.email("foo@bar").emailValidated(true).unofficialBalance(validBalance)
      val response = accountManager.canRequestPayment(account)
      
      response must be(PaymentAllowed)
    }
  }
}



object AccountManager_requestPaymentTests extends Specification with Mockito {
  "AccountManager.requestPayment" should {
    val config = Config.fromLiftProps
    val validBalance = 50
    val invalidBalance = validBalance - 0.01


    "Ignore non-validated accounts" in {
      val mockEmailManager = mock[EmailManager]
      val accountManager = new AccountManager(config, mockEmailManager)
      val account = Account.create.email("foo@bar").emailValidated(false).unofficialBalance(validBalance)

      accountManager.requestPayment(account)
      
      there was no(mockEmailManager).send(any[String], any[EmailTemplate])    
    }
    
    "Ignore accounts with inadequate balance" in {
      val mockEmailManager = mock[EmailManager]
      val accountManager = new AccountManager(config, mockEmailManager)
      val account = Account.create.email("foo@bar").emailValidated(true).unofficialBalance(invalidBalance)

      accountManager.requestPayment(account)
      
      there was no(mockEmailManager).send(any[String], any[EmailTemplate])    
    }
    
    "Notify admin for validated accounts with adequate balance" in {
      val mockEmailManager = mock[EmailManager]
      val accountManager = new AccountManager(config, mockEmailManager)
      val account = Account.create.email("foo@bar").emailValidated(true).unofficialBalance(validBalance)

      accountManager.requestPayment(account)
      
      there was one(mockEmailManager).send(_eq(config.paymentRequestEmail), any[PaymentRequestEmail])    
    }
  }
}
