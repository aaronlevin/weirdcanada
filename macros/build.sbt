name := "Weird Canada Macros"

version := "0.0.1"

organization := "org.weirdcanada.site"

scalaVersion := "2.10.3"

resolvers ++= Seq("snapshots"     at "http://oss.sonatype.org/content/repositories/snapshots",
                "releases"        at "http://oss.sonatype.org/content/repositories/releases"
                )


scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

scalacOptions in Test ++= Seq("-Yrangepos")

libraryDependencies ++= {
  Seq(
    "org.scala-lang" % "scala-reflect" % "2.10.3"
  )
}
