import AssemblyKeys._

name := "Weird Canada Common Ghetto"

version := "0.0.1"

organization := "org.weirdcanada.common"

scalaVersion := "2.10.3"

resolvers ++= Seq("snapshots"     at "http://oss.sonatype.org/content/repositories/snapshots",
                "releases"        at "http://oss.sonatype.org/content/repositories/releases"
                )

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

scalacOptions in Test ++= Seq("-Yrangepos")

libraryDependencies ++= {
  val liftVersion = "2.5-RC1"
  Seq(
    "net.liftweb"       %% "lift-webkit"        % liftVersion        % "compile",
    "org.eclipse.jetty" % "jetty-server"        % "9.0.3.v20130506" ,
    "org.eclipse.jetty" % "jetty-webapp"        % "9.0.3.v20130506" ,
    "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016"  artifacts Artifact("javax.servlet", "jar", "jar"),
    "org.scalaz"        %% "scalaz-core" % "7.0.5",
    "org.specs2" %% "specs2" % "2.3.1" % "test",
    "com.h2database"    % "h2"                  % "1.3.167",
    "io.argonaut"       %% "argonaut"           % "6.0.1",
    "commons-codec" % "commons-codec" % "1.9"
  )
}
