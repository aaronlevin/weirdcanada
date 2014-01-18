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


val Server = config("server") extend(Compile)

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
  inConfig(Server)(
    baseAssemblySettings ++ 
    inTask(assembly)(mainClass := Some("org.weirdcanada.distro.server.WeirdCanadaDistroServer")) ++
    assemblyMergeSettings ++
    assemblyNoTestSetting ++
    Seq(jarName in assembly := "weirdcanada-distro-assembly-1.0.jar")
  ) ++
  inConfig(AlbumApp)(
    baseAssemblySettings ++ 
    inTask(assembly)(mainClass := Some("org.weirdcanada.distro.tools.AlbumApp")) ++
    assemblyMergeSettings ++
    assemblyNoTestSetting ++
    Seq(jarName in assembly := "weirdcanada-albumapp-assembly-1.0.jar")
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
    "postgresql" % "postgresql" % "9.1-901.jdbc4"
  )
}

