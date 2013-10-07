package bootstrap.liftweb

import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._
import js.jquery.JQueryArtifacts
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import net.liftweb.mapper._
import net.liftmodules.JQueryModule
import org.weirdcanada.distro.page._
import org.weirdcanada.distro.data._
import org.weirdcanada.distro.snippet._
import org.weirdcanada.distro.{DistroSiteMapBuilder, SnippetDispatch, Config}
import org.weirdcanada.distro.service.Service
import org.weirdcanada.distro.job.ShopifyClient
import org.weirdcanada.distro.util.EmailFactory
import java.io.FileInputStream

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot extends Bootable with Loggable {
  def boot {
    try {
      internalBoot
    }
    catch {
      case t: Throwable =>
        t.printStackTrace
        sys.exit
    }
  }

  /**
   * Call internalBoot from boot, wrapped in an exception trap so that any failures during initialization will
   * be caught and noticed right away. I've been burned too many times by having the service apparently start
   * up but then exhibit strange behaviour (like snippets classes will fail to resolve all of a sudden)
   */
  private def internalBoot = {
    val config = Config.fromLiftProps
    val emailFactory = EmailFactory.getSslEmailFactory(config)
    val shopifyClient = ShopifyClient(config)
    val service = new Service(config, emailFactory)
    
    service.DatabaseManager.connect
    service.DatabaseManager.createSchema
    
    // where to search snippet
    LiftRules.addToPackages("org.weirdcanada.distro.snippet")
    LiftRules.addToPackages("org.weirdcanada.distro.page")

    // Snippets
    LiftRules.snippetDispatch.append(SnippetDispatch(service, shopifyClient))

    // Build SiteMap
    def sitemap = DistroSiteMapBuilder(service).toSiteMap
    LiftRules.setSiteMapFunc(() => sitemap)


    LiftSession.onSetupSession = (service.SessionManager.onSessionBegin _) :: LiftSession.onSetupSession
    LiftSession.onShutdownSession = (service.SessionManager.onSessionEnd _) :: LiftSession.onShutdownSession
    LiftSession.onBeginServicing = (service.SessionManager.onSessionRequest _) :: LiftSession.onBeginServicing
    
    //LiftRules.onBeginServicing.prepend(service.onRequestBegin)
    LiftRules.onEndServicing.append(service.StatsManager.onRequestEnd)
    
    
    LiftRules.exceptionHandler.prepend {
      case (runMode, request, exception) =>
        // TODO: log error (e.g. to Airbrake)
        logger.error("Failed at: " + request.uri, exception)
        InternalServerErrorResponse()
    }
    
    //Init the jQuery module, see http://liftweb.net/jquery for more information.
    LiftRules.jsArtifacts = JQueryArtifacts
    JQueryModule.InitParam.JQuery=JQueryModule.JQuery172
    JQueryModule.init()

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)
    
    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))    

    // Make a transaction span the whole HTTP request
    S.addAround(DB.buildLoanWrapper)
  }
}
