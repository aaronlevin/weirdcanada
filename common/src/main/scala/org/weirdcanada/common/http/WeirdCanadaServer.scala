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
import org.eclipse.jetty.server.session.SessionHandler
import org.eclipse.jetty.servlet.{DefaultServlet, FilterHolder, FilterMapping,
ServletContextHandler, ServletHandler}
import org.eclipse.jetty.util.thread.QueuedThreadPool
import org.eclipse.jetty.webapp.WebAppContext

// scala
import scala.reflect.ClassTag

abstract class WeirdCanadaServer[A : ClassTag] {

  // defualt should be something like src/main/webapp
  val webAppName: String

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

    val tp = new QueuedThreadPool()
    tp.setMinThreads(10)
    tp.setMaxThreads(200)

    val server = new Server(tp)
    val httpServerConnector = getHttpConnector(server)
    server.setConnectors(Array(httpServerConnector))


    val context = new ServletContextHandler(ServletContextHandler.SESSIONS)
    context.setResourceBase("src/main/resources/%s".format(webAppName))
    context.setContextPath("/")

    context.getServletContext.getContextHandler.setMaxFormContentSize(10000000)
    context.getSessionHandler.getSessionManager.setSessionIdPathParameterName("none")
    context.getSessionHandler.getSessionManager.setMaxInactiveInterval(30*60)


    // Use lift to filter all requests
    val filter = new FilterHolder(classOf[net.liftweb.http.LiftFilter])
    filter.setInitParameter("bootloader", classTag.runtimeClass.getName)
    context.addFilter(filter, "/*", EnumSet.of(DispatcherType.REQUEST))
    context.addServlet(classOf[DefaultServlet], "/")

    server.setHandler(context)

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
    lazy val webAppName = appLocation
  }

}
