import AssemblyKeys._

name := "Dynamic Forms"

version := "0.0.1"

organization := "org.weirdcanada.site"

scalaVersion := "2.10.3"

resolvers ++= Seq("snapshots"     at "http://oss.sonatype.org/content/repositories/snapshots",
                "releases"        at "http://oss.sonatype.org/content/repositories/releases"
                )

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

libraryDependencies ++= {
  val liftVersion = "2.5-RC1"
  Seq(
    "net.liftweb"       %% "lift-webkit"        % liftVersion        % "compile",
    "net.liftweb"       %% "lift-mapper"        % liftVersion        % "compile",
    "net.liftmodules"   %% "lift-jquery-module" % (liftVersion + "-2.2"),
    "ch.qos.logback"    % "logback-classic"     % "1.0.6",
    "org.specs2"        %% "specs2"             % "1.13"           % "test",
    "com.h2database"    % "h2"                  % "1.3.167",
    "postgresql"        % "postgresql"          % "9.1-901.jdbc4",
    "org.scalaz"        %% "scalaz-core" % "7.0.5",
    "joda-time"         % "joda-time"           % "2.2",
    "org.joda"          % "joda-convert"        % "1.2"
  )
}

