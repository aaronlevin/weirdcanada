import sbt._
import Keys._

object MainProject extends Build {

 lazy val weirdcanadaSite = Project(
    id = "site",
    base = file("site")
  ) dependsOn(
    common % "compile->compile",
    dynamicForm % "compile->compile"
  )

 lazy val weirdcanadaDistro = Project(
    id = "distro",
    base = file("distro")
  ) dependsOn(common % "compile->compile")

  lazy val dynamicForm = Project(
    id = "dynamicform",
    base = file("dynamicform")
  )

  lazy val common = Project(
    id = "common",
    base = file("common")
  ) dependsOn(macros % "compile->compile")

  lazy val macros = Project(
    id = "macros",
    base = file("macros")
  )
 
}
