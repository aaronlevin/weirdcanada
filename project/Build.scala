import sbt._
import Keys._

object MainProject extends Build {

 lazy val weirdcanadaSite = Project(
    id = "site",
    base = file("site")
  ) dependsOn(dynamicForm % "compile->compile")

 lazy val weirdcanadaDistro = Project(
    id = "distro",
    base = file("distro")
  )

  lazy val dynamicForm = Project(
    id = "dynamicform",
    base = file("dynamicform")
  )
 
}
