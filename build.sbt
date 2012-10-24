name := "hax"

version := "2.0.0"

scalaVersion := "2.10.0-RC1"

scalacOptions ++= Seq(
  "-deprecation",
  "-Xexperimental"
)

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "pircbot" % "pircbot" % "1.5.0",
  "com.typesafe" % "config" % "0.5.2",
  "com.typesafe" % "slick_2.10.0-M7" % "0.11.1",
  "org.slf4j" % "slf4j-nop" % "1.6.6",
  "org.xerial" % "sqlite-jdbc" % "3.7.2",
  "org.jsoup" % "jsoup" % "1.6.3",
  "net.liftweb" % "lift-json_2.9.1" % "2.4",
  "org.joda" % "joda-convert" % "1.2",
  "joda-time" % "joda-time" % "2.1",
  "org.apache.commons" % "commons-lang3" % "3.1"
)
