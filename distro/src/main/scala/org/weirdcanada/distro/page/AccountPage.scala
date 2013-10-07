package org.weirdcanada.distro.page

import net.liftweb.util.Helpers._
import org.weirdcanada.distro.service.Service
import org.weirdcanada.distro.data.Account
import net.liftweb.sitemap.Menu
import net.liftweb.sitemap.Loc
import org.weirdcanada.distro.DistroSiteMapBuilder
import net.liftweb.sitemap.Menu.ParamMenuable
import net.liftweb.sitemap.LocPath.stringToLocPath
import org.weirdcanada.distro.snippet.{EditableAccountRenderer, ConsignedItemListRenderer}
import org.weirdcanada.distro.snippet.AddConsignedItemRenderer

class AccountPage(service: Service, account: Account)
  extends DistroPage
  with EditableAccountRenderer
  with ConsignedItemListRenderer
  with AddConsignedItemRenderer
{
  def render = {
    ".account" #> renderAccount(account) &
    ".consigned-item-list" #> renderConsignedItemList(account.consignedItems.toList) &
    "*" #> renderAddConsignedItem(account)
  }
}

object AccountPage {
  var menu: ParamMenuable[Account] = null // ugly hack to get around SiteMap limitations in Lift Framework
  def toMenu(service: Service) = {
    menu = Menu.param[Account]("Account", "account", fetchAccount, _.id.is.toString) / "admin" / "account" >> Loc.Hidden >> DistroSiteMapBuilder.mustBeAdmin(service)
    menu
  }
  
  private def fetchAccount(idString: String) = synchronized {
    Account.findByKey(idString.toLong) 
  }
  
  // Convenience method for when you have an ID but don't want to dig up a account object.
  // called by the Admin Dashboard Page when listing account sessions
  def calcHref(accountId: Option[Long]) = {
     accountId.map("/admin/account/%s".format(_)).getOrElse("#")
   }
}