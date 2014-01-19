package org.weirdcanada.site.http

import org.weirdcanada.common.http.WeirdCanadaServer
import bootstrap.liftweb.{Boot => BootClass}

object WeirdCanadaSiteServer {

  private val server: WeirdCanadaServer[BootClass] = WeirdCanadaServer("site")

  def main(args: Array[String]) = server.main(args)

}
