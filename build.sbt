import AssemblyKeys._

lazy val site =
  project
    .in(file("site"))
    .dependsOn(
      common % "compile->compile",
      dynamicform % "compile->compile"
    )

lazy val distro =
  project
    .in(file("distro"))
    .dependsOn(
      common % "compile->compile",
      dynamicform % "compile->compile"
    )

lazy val dynamicform = project.in(file("dynamicform"))

lazy val common =
  project
    .in(file("common"))
    .dependsOn(macros % "compile->compile")

lazy val macros =
  project
    .in(file("macros"))
    .settings(assemblySettings: _*)
