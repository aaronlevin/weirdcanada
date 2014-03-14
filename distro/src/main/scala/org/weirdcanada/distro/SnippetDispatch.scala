package org.weirdcanada.distro

import org.weirdcanada.distro.page._
import org.weirdcanada.distro.snippet._
import net.liftweb.http.DispatchSnippet
import org.weirdcanada.distro.page.snippet._
import org.weirdcanada.distro.job.ShopifyClient
import org.weirdcanada.distro.service.Service

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
    case "AddArtistPage" => new AddArtistPage(service)
    case "AddAlbumPage" => new AddAlbumPage(service)
    case "AddPublisherPage" => new AddPublisherPage(service)
    case "AddConsignedItemPage" => new AddConsignedItemPage(service)
    case "ForgotPasswordPage" => new ForgotPasswordPage(service)
    case "UpdatePasswordPage" => new UpdatePasswordPage(service, service.SessionManager.current.account)
    case "Payments" => new PaymentsSnippet(service)
  }
}
