import AssemblyKeys._

name := "Weird Canada Common Ghetto"

version := "0.0.1"

organization := "org.weirdcanada.site"

scalaVersion := "2.10.1"

resolvers ++= Seq("snapshots"     at "http://oss.sonatype.org/content/repositories/snapshots",
                "releases"        at "http://oss.sonatype.org/content/repositories/releases"
                )

seq(com.github.siasia.WebPlugin.webSettings :_*)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

scalacOptions in Test ++= Seq("-Yrangepos")

libraryDependencies ++= {
  val liftVersion = "2.5-RC1"
  Seq(
    "net.liftweb"       %% "lift-webkit"        % liftVersion        % "compile",
    "org.eclipse.jetty" % "jetty-server"        % "9.0.3.v20130506" ,
    "org.eclipse.jetty" % "jetty-webapp"        % "9.0.3.v20130506" ,
    "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016"  artifacts Artifact("javax.servlet", "jar", "jar"),
    "org.eclipse.jetty" % "jetty-server"        % "8.1.7.v20120910" % "container",
    "org.eclipse.jetty" % "jetty-servlet"        % "8.1.7.v20120910" % "container",
    "org.eclipse.jetty" % "jetty-webapp"        % "8.1.7.v20120910"  % "container,test",
    "org.scalaz"        %% "scalaz-core" % "7.0.3",
    "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "container,test" artifacts Artifact("javax.servlet", "jar", "jar"),
    "org.specs2" %% "specs2" % "2.3.1" % "test"
  )
}
