import AssemblyKeys._

name := "weirdcanada-admin"

version := "0.0.1"

organization := "org.weirdcanada.site"

scalaVersion := "2.10.3"

resolvers ++= Seq("snapshots"     at "http://oss.sonatype.org/content/repositories/snapshots",
                "releases"        at "http://oss.sonatype.org/content/repositories/releases"
                )


unmanagedResourceDirectories in Test <+= (baseDirectory) { _ / "src/main/webapp" }

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

assemblySettings

mainClass in assembly := Some("org.weirdcanada.site.http.WeirdCanadaSiteServer")

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case "about.html" => MergeStrategy.discard
    case x => old(x)
  }
}

jarName in assembly := "weirdcanada-site.jar"

TaskKey[Unit]("build", "Assembly and copy jars") <<= (baseDirectory, assembly) map { (bDir, fatJar) =>
  println("Copying jars")
  println(fatJar)
  println("===>")
  println(bDir / "jars" / fatJar.name)
  IO.copyFile(fatJar, bDir / "jars" / fatJar.name)
}

libraryDependencies ++= {
  val liftVersion = "2.5-RC1"
  Seq(
    "net.liftweb"       %% "lift-webkit"        % liftVersion        % "compile",
    "net.liftweb"       %% "lift-mapper"        % liftVersion        % "compile",
    "net.liftmodules"   %% "lift-jquery-module" % (liftVersion + "-2.2"),
    "org.eclipse.jetty" % "jetty-server"        % "9.0.3.v20130506" ,
    "org.eclipse.jetty" % "jetty-webapp"        % "9.0.3.v20130506" ,
    "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016"  artifacts Artifact("javax.servlet", "jar", "jar"),
    "ch.qos.logback"    % "logback-classic"     % "1.0.6",
    "org.specs2"        %% "specs2"             % "1.13"           % "test",
    "com.h2database"    % "h2"                  % "1.3.167",
    "postgresql"        % "postgresql"          % "9.1-901.jdbc4",
    "org.scalaz"        %% "scalaz-core" % "7.0.5",
    "joda-time"         % "joda-time"           % "2.2",
    "org.joda"          % "joda-convert"        % "1.2",
    "org.clapper"       % "markwrap_2.10"       % "1.0.1",
    "net.databinder.dispatch" %% "dispatch-core" % "0.10.1"
  )
}

