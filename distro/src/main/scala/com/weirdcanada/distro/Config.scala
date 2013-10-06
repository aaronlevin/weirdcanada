package org.weirdcanada.distro

import net.liftweb.util.Props
import net.liftweb.common.Full
import java.io.FileInputStream

trait Config {
  def distroEndPoint: String

  // Database Connection Info
  def dbDriver: String
  def dbUrl: String
  def dbUsername: String
  def dbPassword: String
  
  // Shopify Credentials
  def shopifyApiKey: String
  def shopifySecret: String
  def shopifyPassword: String
  def shopifyEndPoint: String
  
  def shopifyPollingInterval: Int
  
  // Email credentials
  def smtpHost: String
  def smtpPort: String
  def smtpUsername: String
  def smtpPassword: String
  def smtpOverrideTo: String
  
  def paymentRequestEmail: String
  
  // TODO: airbrake credentials
}


class LiftPropsConfig extends Config {
  private def get(name: String) = Props.get(name).getOrElse(sys.error(name + " missing from props file"))

  override val distroEndPoint = get("distroEndPoint")

  // Database Connection Info
  override val dbDriver = get("dbDriver")
  override val dbUrl = get("dbUrl")
  override val dbUsername = get("dbUsername")
  override val dbPassword = get("dbPassword")
  
  // Shopify Credentials
  override val shopifyApiKey = get("shopifyKey")
  override val shopifySecret = get("shopifySharedSecret")
  override val shopifyPassword = get("shopifyPassword")
  override val shopifyEndPoint = get("shopifyEndPoint")
  
  override val shopifyPollingInterval = get("shopifyPollingInterval").toInt
  
  // Email Credentials
  override val smtpHost = get("smtpHost")
  override val smtpPort = get("smtpPort")
  override val smtpUsername = get("smtpUsername")
  override val smtpPassword = get("smtpPassword")
  override val smtpOverrideTo = get("smtpOverrideTo") // Used for testing... always send emails to this address

  // Where payment requests are sent
  override val paymentRequestEmail = get("paymentRequestEmail")
}

object Config {
  sys.props.get("props.file").foreach(filename => 
    Props.whereToLook = () => List((filename, () => Full(new FileInputStream(filename))))
  )
  
  def fromLiftProps = new LiftPropsConfig
}