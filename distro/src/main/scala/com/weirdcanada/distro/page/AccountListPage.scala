package com.weirdcanada.distro.page

import net.liftweb.util.Helpers._
import net.liftweb.mapper.{MaxRows => Limit, _}
import com.weirdcanada.distro.data.Account
import com.weirdcanada.distro.service.Service
import com.weirdcanada.distro.snippet.AccountRenderer


class AccountListPage(service: Service) extends DistroPage with AccountRenderer {
  // TODO: loading more and sorting
  def render = {
    ".account" #> Account.findAll(Limit(100)).map(renderAccount)
  }
}