package org.weirdcanada.distro.server

import org.weirdcanada.common.http.WeirdCanadaServer
import bootstrap.liftweb.{Boot => BootClass}

object WeirdCanadaDistroServer {

  private val server: WeirdCanadaServer[BootClass] = WeirdCanadaServer("distro/src/main/resources/distro")

  def main(args: Array[String]) = server.main(args)

}
