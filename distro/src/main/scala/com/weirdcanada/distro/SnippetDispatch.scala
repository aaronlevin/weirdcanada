package com.weirdcanada.distro

import com.weirdcanada.distro.page._
import com.weirdcanada.distro.snippet._
import net.liftweb.http.DispatchSnippet
import com.weirdcanada.distro.page._
import com.weirdcanada.distro.job.ShopifyClient
import com.weirdcanada.distro.service.Service

object SnippetDispatch {
  def apply(service: Service, shopifyClient: ShopifyClient): PartialFunction[String, DispatchSnippet] = {
    case "IndexPage" => new IndexPage(service)
    case "LoginPage" => new LoginPage(service)
    case "ConsignorPage" => new ConsignorPage(service)
    case "DashboardPage" => new DashboardPage(service, shopifyClient)
    case "AccountListPage" => new AccountListPage(service)
    case "AccountPage" => new AccountPage(service, AccountPage.menu.currentValue.openOrThrowException("User account URL required"))
    case "MyAccountPage" => new AccountPage(service, service.SessionManager.current.account)
    case "RegisterPage" => new RegisterPage(service)
    case "CheckYourInboxPage" => new CheckYourInboxPage(service)

    case "Chart" | "chart" => new ChartSnippet(service)
    case "PageView" | "pageview" => new PageViewSnippet(service)
    case "HasRole" | "hasrole" => new HasRoleSnippet(service)
  }
}