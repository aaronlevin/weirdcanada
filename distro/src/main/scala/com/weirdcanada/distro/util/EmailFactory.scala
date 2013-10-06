package com.weirdcanada.distro.util

import java.util.Properties
import javax.mail.{Message, MessagingException, PasswordAuthentication, Session, Transport, Authenticator}
import javax.mail.internet.InternetAddress
import javax.mail.internet.MimeMessage
import com.weirdcanada.distro.Config
import scala.xml.NodeSeq
import net.liftweb.util.Props


object EmailFactory {
  def getSslEmailFactory(config: Config) = new SslEmailFactory(config)
}

trait EmailFactory {
  def send(to: String, from: String, subject: String, body: NodeSeq): Unit
}

case object NullEmailFactory extends EmailFactory {
  def send(to: String, from: String, subject: String, body: NodeSeq) {
    //println("[Null Email] to: %s, from: %s, subject: %s".format(to, from, subject))
  }
}

class SslEmailFactory(config: Config) extends EmailFactory {
  override def send(to: String, from: String, subject: String, body: NodeSeq) {
    val props = new Properties();
    props.put("mail.smtp.host", config.smtpHost);
    props.put("mail.smtp.socketFactory.port", config.smtpPort);
    props.put("mail.smtp.socketFactory.class", "javax.net.ssl.SSLSocketFactory");
    props.put("mail.smtp.auth", "true");
    props.put("mail.smtp.port", config.smtpPort);
 
    val session = Session.getDefaultInstance(props,
      new Authenticator {
        override def getPasswordAuthentication = {
          new PasswordAuthentication(config.smtpUsername, config.smtpPassword)
        }
      })
 
    try {
      val message = new MimeMessage(session)
      message.setFrom(new InternetAddress(from))
      if (config.smtpOverrideTo.isEmpty && Props.productionMode)
        message.setRecipients(Message.RecipientType.TO, to) //InternetAddress.parse(to))
      else
        message.setRecipients(Message.RecipientType.TO, config.smtpOverrideTo)      
      message.setSubject(subject)
      //message.setText(body);
      message.setContent(body.toString, "text/html")
      
      Transport.send(message);
 
      System.out.println("Done");
 
    } catch {
      case e: MessagingException =>
        // TODO: log with Airbrake
        throw new RuntimeException(e)
    }
  }
}