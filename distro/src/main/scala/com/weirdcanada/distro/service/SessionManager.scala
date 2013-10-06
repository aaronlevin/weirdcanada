package org.weirdcanada.distro.service

import org.weirdcanada.distro.DistroSession
import net.liftweb.http.{LiftSession, Req, S}
import net.liftweb.common.{Full, Empty}
import org.weirdcanada.distro.data.Account
import net.liftweb.http.provider.HTTPCookie
import net.liftweb.util.Props

class SessionManager {
  val sessions = scala.collection.mutable.HashMap.empty[String, DistroSession]

  def onSessionBegin(session: LiftSession) {
    val distroSession = DistroSession(session)
    synchronized {
      sessions += session.uniqueId -> distroSession
    }
    
    // Perform a cookie login if the current user is not logged in but they have a wcdid cookie
    if (!distroSession.isLoggedIn) {
      for {
        wcdid <- S.cookieValue("wcdid")
        account <- Account.findByWcdId(wcdid)
      }
      yield {
        if (wcdid.length > 0)
          distroSession.logIn(account)
        else
          S.addCookie(HTTPCookie("wcdid", Full(""), if (Props.devMode) Empty else Full(S.hostName), Full("/"), Full(0), Empty, Empty))
      }
    }
  }
  
  def onSessionEnd(session: LiftSession) {
    synchronized {
      sessions -= session.uniqueId 
    }
  }

  def onSessionRequest(session: LiftSession, req: Req) {
    /*
    val distroSession = synchronized {
      sessions.getOrElseUpdate(session.uniqueId, DistroSession(session))
    }
    ...
    */
  }
  
  def getActiveSessions = synchronized {
    sessions.toList
  }

  def current = DistroSession.current
}
