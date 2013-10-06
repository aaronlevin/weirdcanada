package org.weirdcanada.distro.service

import org.weirdcanada.distro.util.EmailFactory
import org.weirdcanada.distro.Config


class Service(val config: Config, emailFactory: EmailFactory) {
  val StatsManager = new StatsManager(config)
  val DatabaseManager = new DatabaseManager(config)
  val EmailManager = new EmailManager(config, emailFactory)
  val AccountManager = new AccountManager(config, EmailManager)
  val SessionManager = new SessionManager()
}
