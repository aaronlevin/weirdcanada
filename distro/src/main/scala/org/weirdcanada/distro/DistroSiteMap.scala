package org.weirdcanada.distro

import net.liftweb.sitemap.{Loc, Menu, SiteMap}
import Loc.{LocGroup, LocSnippets, Snippet}
import net.liftweb.common._
import net.liftweb.http._
import org.weirdcanada.distro.api.shopify.Shopify
import org.weirdcanada.distro.data.{Account, Artist, ArtistData, Album, AlbumData, ConsignedItem, Publisher, PublisherData}
import Account.AccountData
import ConsignedItem.ConsignedItemData
import org.weirdcanada.distro.page.{AccountPage}
import org.weirdcanada.distro.page.snippet.EditArtistPage
import org.weirdcanada.distro.service.Service
import scala.xml.NodeSeq

class DistroSiteMapBuilder(service: Service, shopify: Shopify) {
  implicit def svc = service
  import DistroSiteMapBuilder._

  val editArtistPage = Menu.param[(Artist,ArtistData)](
    "EditArtist",
    "Edit Artist",
    id =>  Artist.findByStringId(id).map { a => (a, Artist.toData(a)) },
    (tuple) => tuple._2.id.toString
  ) / "admin" / "edit-artist" >> mustBeAdmin >> LocGroup("actions")

  val editPublisherPage = Menu.param[(Publisher,PublisherData)](
    "EditPublisher",
    "Edit Publisher",
    id =>  Publisher.findByStringId(id).map { a => (a, Publisher.toData(a)) },
    (tuple) => tuple._2.id.toString
  ) / "admin" / "edit-publisher" >> mustBeAdmin >> LocGroup("actions")

  val editAlbumPage = Menu.param[(Album,AlbumData, Shopify)](
    "EditAlbum",
    "Edit Album",
    id =>  Album.findByStringId(id).map { a => (a, Album.toData(a), shopify) },
    (tuple) => tuple._2.id.toString
  ) / "admin" / "edit-album" >> mustBeAdmin >> LocGroup("actions")

  val editConsignedItemPage = Menu.param[(ConsignedItem, ConsignedItemData,Shopify)](
    "EditConsignedItem",
    "Edit Consigned Item",
    id => ConsignedItem.findByStringId(id).map { c => (c, ConsignedItem.toData(c), shopify) },
    (tuple) => tuple._2.id.toString
  ) / "admin" / "edit-consigned-item" >> mustBeAdmin >> LocGroup("actions")

  val editConsignorPage = Menu.param[(Account, AccountData)](
    "EditAccountPage",
    "Edit Account Page",
    id => Account.findByStringId(id).map { a => (a, Account.toData(a)) },
    (tuple) => tuple._2.toString
  ) / "admin" / "edit-consignor" >> mustBeAdmin >> LocGroup("actions")

  def toSiteMap =
    SiteMap(
      Menu.i("Home") / "index",

      Menu.i("Register") / "register" >> mustBeVisitor,


      Menu.i("Check Your Inbox") / "check-your-inbox"
        >> mustBeLoggedIn
        >> Loc.Hidden,

      // Actions
      Menu.i("Request Return") / "request-return"
        >> mustBeLoggedIn
        >> Loc.Hidden
        >> Loc.EarlyResponse(() => requestPaymentResponse)
        >> LocGroup("actions"),

      Menu.i("Add an Artist") / "admin" / "add-artist"
        >> mustBeAdmin
        >> LocGroup("actions"),


      Menu.i("Add a Publisher") / "admin" / "add-publisher"
        >> mustBeAdmin
        >> LocGroup("actions"),

      Menu.i("Add an Album") / "admin" / "add-album"
        >> mustBeAdmin
        >> LocGroup("actions"),

      Menu.i("Add Consign Item") / "admin" / "add-consigned-item"
        >> mustBeAdmin
        >> LocGroup("actions"),

      Menu.i("Edit Items") / "admin" / "edit"
        >> mustBeAdmin
        >> LocGroup("actions"),

     Menu.i("My Account") / "my-account" 
        >> mustBeLoggedIn
        >> LocGroup("actions"),

      Menu.i("Forgot Password") / "forgot-password" 
        >> mustBeVisitor 
        >> LocGroup("actions"),

      Menu.i("Update Password") / "update-password" 
        >> mustBeLoggedIn 
        >> LocGroup("actions"),

      editArtistPage,
      editPublisherPage,
      editAlbumPage,
      editConsignedItemPage,
      AccountPage.toMenu(service),

      Menu.i("Dashboard") / "admin" / "dashboard" >> mustBeAdmin,
      Menu.i("Accounts") / "admin" / "account-list" >> mustBeAdmin,
      Menu.i("Add a New Record") / "admin" / "add-records" 
        >> mustBeAdmin,


      Menu.i("Request Payment") / "request-payment"
        >> mustBeLoggedIn
        >> Loc.Hidden
        >> Loc.EarlyResponse(() => requestPaymentResponse),

      Menu.i("Logout") / "logout" >> mustBeLoggedIn
        >> Loc.EarlyResponse(() => {
          service.SessionManager.current.logOut
          S.redirectTo("/")
        }),

      Menu.i("Confirm Registration") / "confirm-registration"
        >> Loc.Hidden
        >> Loc.EarlyResponse(() => confirmRegistration)
    )

    def confirmRegistration = {
      for {
        email <- S.param("email")
        key <- S.param("key")
      }
      yield {
        service.AccountManager.confirmRegistration(email, key)
      }

      Full(RedirectResponse("/"))
    }

    def requestPaymentResponse = {
      for {
        account <- service.SessionManager.current.accountOpt
      }
      yield {
        service.AccountManager.requestPayment(account)
      }

      Full(RedirectResponse("/"))
    }
}

object DistroSiteMapBuilder {
  def apply(service: Service, shopify: Shopify) = new DistroSiteMapBuilder(service, shopify)

  def mustBeVisitor(implicit service: Service) = Loc.TestAccess(() => if (!service.SessionManager.current.isLoggedIn) Empty else Full(RedirectResponse("/")))
  def mustBeLoggedIn(implicit service: Service)  = Loc.TestAccess(() => if (service.SessionManager.current.isLoggedIn) Empty else Full(RedirectResponse("/")))
  def mustBeAdmin(implicit service: Service)  = Loc.TestAccess(() => if (service.SessionManager.current.isAdmin) Empty else Full(RedirectResponse("/")))
}
