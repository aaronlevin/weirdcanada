package org.weirdcanada.util

object EmailUtil {

  /**
   * Parse an email into `name`, `hostName`, and `tld`. 
   *
   * `aaron@cool.com` -> (aaron, cool, com)
   *
   * @param email the email 
   * @return a triple of host, hostName, and tld
   */
  def parseEmail(email: String): Option[(String, String, String)] = email.split("@").toList match {
    case Nil => None
    case name :: Nil => None
    case name :: hostName :: Nil => 
      hostName.split(".").toList match {
        case host :: tld :: Nil => Some((name, host, tld))
        case _ => None
      }
    case _ => None
  }

}

