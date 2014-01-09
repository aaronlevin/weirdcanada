package org.weirdcanada.distro.service

import scala.xml.NodeSeq
import org.specs2.mutable.Specification
import org.specs2.execute.{AsResult, Result}
import org.specs2.mock._
import org.mockito.Matchers.{eq => _eq, _}
import org.weirdcanada.distro.util.{EmailFactory, NullEmailFactory}
import org.weirdcanada.distro.data.Account
import org.weirdcanada.distro.Config
import org.weirdcanada.distro.service._
import org.specs2.matcher.Matcher
import org.mockito.ArgumentMatcher


object EmailManager_Tests extends Specification with Mockito {
  val config = Config.fromLiftProps
  val toAddress = "test-user@weirdcanada.com"
  val name = "Mr. Test User"
  val confirmUrl = "http://confirmation-url"
  val accountUrl = "/admin/account/foo"
  val balance = BigDecimal(56.78)
  val expectedBalanceString = "$56.78"

  class NodeSeqMatcher(fn: NodeSeq => Boolean) extends ArgumentMatcher[NodeSeq] {
    def matches(ns: Any) = {
      ns match {
        case ns: NodeSeq =>
          fn(ns)
          
        case _ => false
      }
    }
  }
  
  private def nodeSeqContains(content: String*) = anArgThat(new NodeSeqMatcher(ns => {
    val s = ns.toString
    content.forall(s.contains)
  }))
  
  
  "EmailManager.send ConfirmationRegistration" should {
    val mockEmailFactory = mock[EmailFactory]
    val emailManager = new EmailManager(config, mockEmailFactory)
    
    "contain confirmation link and name" in {
      emailManager.send(toAddress, ConfirmRegistrationEmail(confirmUrl, name))
      
      there was one(mockEmailFactory).send(_eq(toAddress), _eq(config.smtpUsername), _eq("Registration Confirmation"), nodeSeqContains(confirmUrl, name))
    }
  }
  
  
  "EmailManager.send PaymentRequest" should {
    val mockEmailFactory = mock[EmailFactory]
    val emailManager = new EmailManager(config, mockEmailFactory)
    
    "contain account name, url and balance" in {
      emailManager.send(config.paymentRequestEmail, PaymentRequestEmail(name, accountUrl, balance, 1))
      
      there was one(mockEmailFactory).send(_eq(config.paymentRequestEmail), _eq(config.smtpUsername), _eq("Payment Request"), nodeSeqContains(name, accountUrl, expectedBalanceString))
    }
  }
}
