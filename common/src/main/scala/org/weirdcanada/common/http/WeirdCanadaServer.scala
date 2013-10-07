package org.weirdcanada.common.http

// Java
import javax.servlet.DispatcherType
import java.util.EnumSet

// Lift
import net.liftweb.http.LiftFilter
//import bootstrap.liftweb.{Boot => BootClass}

// 3rd Party
import org.eclipse.jetty.server.{
  Connector
, Handler
, HttpConfiguration
, HttpConnectionFactory
, SecureRequestCustomizer
, Server
, ServerConnector }
import org.eclipse.jetty.server.handler.ContextHandler
import org.eclipse.jetty.servlet.{DefaultServlet, FilterHolder, FilterMapping}
import org.eclipse.jetty.webapp.WebAppContext

// scala
import scala.reflect.ClassTag

abstract class WeirdCanadaServer[A : ClassTag] {

  // defuat should be something like src/main/webapp
  val webAppLocation: String

  val classTag = implicitly[ClassTag[A]]

  // Http Configuration
  private val http_config: HttpConfiguration = {
    val tmp_http_config = new HttpConfiguration()
    tmp_http_config.setSecureScheme("https");
    tmp_http_config.setSecurePort(8443);
    tmp_http_config.setOutputBufferSize(32768);

    tmp_http_config
  }

  // Get HTTP Connector
  private def getHttpConnector(server: Server): ServerConnector = {
    // HTTP connector
    val http: ServerConnector = new ServerConnector(server, new HttpConnectionFactory(http_config))
    http.setPort(8080)
    http.setIdleTimeout(30000)

    http
  }

  // Main
  def main(args: Array[String]) {
    val server = new Server
    val httpServerConnector = getHttpConnector(server)
    server.setConnectors(Array(httpServerConnector))

    val context = new WebAppContext()
    context.setServer(server)
    context.setWar(webAppLocation)

    // Use lift to filter all requests
    val filter = new FilterHolder(classOf[net.liftweb.http.LiftFilter])
    filter.setInitParameter("bootloader", classTag.runtimeClass.getName)
    context.addFilter(filter, "/*", EnumSet.of(DispatcherType.REQUEST))
    context.addServlet(classOf[DefaultServlet], "/")

    val contextHandler: ContextHandler = new ContextHandler();
    contextHandler.setHandler(context)
    server.setHandler(contextHandler)

    try {
      println(">>> XXX STARTING EMBEDDED JETTY SERVER, PRESS ANY KEY TO STOP")
      server.start()
      while (System.in.available() == 0) {
        Thread.sleep(5000)
      }
      server.stop()
      server.join()
    } catch {
      case exc: Exception => {
        exc.printStackTrace()
        System.exit(100)
      }
    }
  }
}

object WeirdCanadaServer {

  def apply[A : ClassTag](appLocation: String): WeirdCanadaServer[A] = new WeirdCanadaServer[A] {
    lazy val webAppLocation = appLocation
  }

}
