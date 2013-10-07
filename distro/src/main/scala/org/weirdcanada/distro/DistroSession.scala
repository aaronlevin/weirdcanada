package org.weirdcanada.distro

import net.liftweb.http.LiftSession
import net.liftweb.http.Req
import net.liftweb.http.SessionVar
import net.liftweb.http.S
import org.weirdcanada.distro.data.UserRole
import org.weirdcanada.distro.data.UserRole.{Value => UserRoleEnum}
import org.weirdcanada.distro.data.Account
import net.liftweb.http.provider.HTTPCookie
import net.liftweb.common.{Full, Empty}
import net.liftweb.util.Props

case class DistroSession(sessionId: String, startTime: Long) {
  def duration = System.currentTimeMillis - startTime
  def timeOnPage = lastRequestTime.map(System.currentTimeMillis - _).getOrElse(0L)

  
  var accountId: Option[Long] = None
  var accountName: Option[String] = None
  var userRole: Option[UserRoleEnum] = Some(UserRole.Visitor)
  var ipAddress: Option[String] = None
  var lastRequestUri: Option[String] = None
  var lastRequestTime: Option[Long] = None

  def accountOpt = accountId.flatMap(Account.findByKey)
  def account = accountOpt.getOrElse(sys.error("Account not logged in"))
  
  var _requestCount = 0;
  def requestCount = _requestCount
  def onPageView(req: Req) = synchronized {
    _requestCount += 1

    ipAddress = Some(req.remoteAddr)
    lastRequestUri = Some(req.uri)
    lastRequestTime = Some(System.currentTimeMillis)
  }
  
  def isLoggedIn = {
    accountId.isDefined
  }
  def isAdmin = {
    isLoggedIn && userRole.map(_ == UserRole.Admin).getOrElse(false)
  }

  def logIn(account: Account) = synchronized {
    // TODO: log the login
    accountId = Some(account.id.is)
    accountName = Some(account.displayName)
    userRole = Some(account.role.is)
    account
  }
  
  def logOut = synchronized {
    // TODO: log the logout
    accountId = None
    accountName = None
    userRole = Some(UserRole.Visitor)
    
    S.addCookie(HTTPCookie("wcdid", Full(""), if (Props.devMode) Empty else Full(S.hostName), Full("/"), Full(0), Empty, Empty))
  }
}

object DistroSession {
  private object _current extends SessionVar[Option[DistroSession]](None)
  def currentOption = _current.is
  def current = currentOption.getOrElse(sys.error("No current session"))
  
  def apply(session: LiftSession): DistroSession = {
    val ds = DistroSession(session.uniqueId, System.currentTimeMillis)
    _current.set(Some(ds))
    ds
  }
}