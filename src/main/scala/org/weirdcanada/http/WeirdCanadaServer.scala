package org.weirdcanada.http

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
import org.eclipse.jetty.webapp.WebAppContext

object WeirdCanadaServer {

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
    context.setWar("src/main/webapp")

    val context0: ContextHandler = new ContextHandler();
    context0.setHandler(context)
    server.setHandler(context0)

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
