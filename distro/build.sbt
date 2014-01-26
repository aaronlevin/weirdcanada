import AssemblyKeys._

name := "weirdcanada-distro"

version := "1.0"

organization := "org.weirdcanada.distro"

scalaVersion := "2.10.3"

resolvers ++= Seq("snapshots"     at "http://oss.sonatype.org/content/repositories/snapshots",
                  "staging"       at "http://oss.sonatype.org/content/repositories/staging",
                  "releases"      at "http://oss.sonatype.org/content/repositories/releases"
                 )

unmanagedResourceDirectories in Test <+= (baseDirectory) { _ / "src/main/webapp" }

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")


val Portal = config("portal") extend(Compile)

val AlbumApp = config("albumapp") extend(Compile)

lazy val assemblyMergeSettings = Seq(
  mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case "about.html" => MergeStrategy.discard
    case x => old(x)
  }}
)

lazy val assemblyNoTestSetting = Seq(
  test in assembly := {}
)

lazy val customAssemblySettings: Seq[Project.Setting[_]] =
  inConfig(Portal)(
    baseAssemblySettings ++ 
    inTask(assembly)(mainClass := Some("org.weirdcanada.distro.server.WeirdCanadaDistroServer")) ++
    assemblyMergeSettings ++
    assemblyNoTestSetting ++
    Seq(jarName in assembly := "weirdcanada-distro.jar") ++
    jrebelSettings ++
    Seq(buildTask)
  ) ++
  inConfig(AlbumApp)(
    baseAssemblySettings ++ 
    inTask(assembly)(mainClass := Some("org.weirdcanada.distro.tools.AlbumApp")) ++
    assemblyMergeSettings ++
    assemblyNoTestSetting ++
    Seq(jarName in assembly := "weirdcanada-albumapp.jar") ++
    Seq(buildTask)
  )

seq(customAssemblySettings: _*)

libraryDependencies ++= {
  val liftVersion = "2.5"
  Seq(
    "net.liftweb"       %% "lift-webkit"        % liftVersion        % "compile",
    "net.liftweb"       %% "lift-mapper"        % liftVersion        % "compile",
    "ch.qos.logback"    % "logback-classic"     % "1.0.6",
    "joda-time"         % "joda-time"           % "2.2",
    "com.h2database"    % "h2"                  % "1.3.167",
    "org.specs2"        % "specs2_2.10"         % "2.1.1"            % "test",
    "org.mockito"       % "mockito-all"         % "1.9.5"            % "test",
    "postgresql"        % "postgresql"          % "9.1-901.jdbc4",
    "io.argonaut"       %% "argonaut"           % "6.0.1"
  )
}

lazy val buildTask =
  TaskKey[Unit]("build", "Assemble and copy jars") <<= (baseDirectory, assembly) map { (bDir, fatJar) =>
    println("Copying %s\n==> %s".format(fatJar, bDir / "jars" / fatJar.name))
    IO.copyFile(fatJar, bDir / "jars" / fatJar.name)
  }
