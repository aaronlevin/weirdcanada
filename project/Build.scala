import sbt._
import Keys._

object MainProject extends Build {

  lazy val weirdcanadaSite = Project(id = "site",
                                     base = file("site"))

  lazy val weirdcanadaDistro = Project(id = "distro",
                                       base = file("distro"))

}
