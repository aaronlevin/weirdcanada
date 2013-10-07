package org.weirdcanada.distro.page

import net.liftweb.util.Helpers._
import net.liftweb.mapper.{MaxRows => Limit, _}
import org.weirdcanada.distro.data.Account
import org.weirdcanada.distro.service.Service
import org.weirdcanada.distro.snippet.AccountRenderer


class AccountListPage(service: Service) extends DistroPage with AccountRenderer {
  // TODO: loading more and sorting
  def render = {
    ".account" #> Account.findAll(Limit(100)).map(renderAccount)
  }
}