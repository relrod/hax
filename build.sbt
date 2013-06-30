name := "hax"

version := "2.0.0"

scalaVersion := "2.10.2"

scalacOptions ++= Seq(
  "-deprecation",
  "-Xexperimental"
)

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "pircbot" % "pircbot" % "1.5.0",
  "com.typesafe" % "config" % "1.0.1",
  "com.typesafe.slick" %% "slick" % "1.0.1",
  "org.slf4j" % "slf4j-nop" % "1.7.5",
  "org.xerial" % "sqlite-jdbc" % "3.7.2",
  "org.jsoup" % "jsoup" % "1.7.2",
  "net.liftweb" % "lift-json_2.9.1" % "2.5",
  "org.joda" % "joda-convert" % "1.3.1",
  "joda-time" % "joda-time" % "2.2",
  "org.apache.commons" % "commons-lang3" % "3.1",
  "me.elrod" %% "pkgwat" % "1.0.2"
)
