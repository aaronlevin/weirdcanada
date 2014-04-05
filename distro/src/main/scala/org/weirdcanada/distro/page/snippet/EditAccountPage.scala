package org.weirdcanada.distro.page.snippet

import net.liftweb.common.{Empty, Full, Failure}
import net.liftweb.http.{DispatchSnippet, RequestVar, SHtml}
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.sitemap.{Loc, Menu}
import net.liftweb.sitemap._
import Loc.LocGroup
import net.liftweb.util.Helpers._
import org.weirdcanada.distro.DistroSiteMapBuilder
import org.weirdcanada.distro.api.shopify.Shopify
import org.weirdcanada.distro.data.{Account, Album, AlbumData}
import Account.AccountData
import org.weirdcanada.distro.tools.UploadAlbumToShopify
import org.weirdcanada.dynamicform.{DynamicFormCreator}
import scala.xml.{NodeSeq, Text}
import scalaz.\/
import scalaz.{\/-,-\/} // Zoidberg

class EditAccountPage(accountDataTuple: (Account, AccountData)) extends DispatchSnippet with DynamicFormCreator  {

  import Account._

  /**
   * Get artist from id in `ArtistData`
   */
  val account = accountDataTuple._1
  val data = accountDataTuple._2

  private val addErrorJs = """(function() { var x = document.getElementById('account-update'); x.className = x.className + " has-error";})();"""
  private val addSuccessJs = """(function() { var x = document.getElementById('account-update'); x.className = x.className + " has-success";})();"""
  private val removeErrorJs = """$('.has-error').removeClass('has-error');"""

  private def updateAccountFunc(data: AccountData, account: Account): JsCmd = {
    Account.updateFromData(data, account) match {
      case \/-(_) => 
        JsCmds.Run(removeErrorJs + addSuccessJs) & 
        JsCmds.SetHtml("account-update-error", Text("Successfully updated account with id %s".format(data.id)))
      case -\/(errorMsg) => 
        JsCmds.Run(addErrorJs) &
        JsCmds.SetHtml("account-update-error", Text("%s".format(errorMsg)))
    }

  }

  /** Dynamic Field stuff
   */
  private object accountState extends RequestVar[AccountData](data)
  def updateState = getUpdateAndSaveFuncForField[AccountData](accountState)
  val renderFunction = renderField(accountState)

  def render = renderFunction andThen {
    "@account-update-button" #> SHtml.ajaxButton("Update", () => updateAccountFunc(accountState.is,account))
  }

  def dispatch = {
    case "render" => render
  }

}
