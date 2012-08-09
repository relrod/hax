name := "hax"

version := "2.0.0"

scalaVersion := "2.9.2"

scalacOptions += "-deprecation -Xexperimental"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "pircbot" % "pircbot" % "1.5.0",
  "com.typesafe" % "config" % "0.5.0",
  "com.typesafe" % "slick_2.10.0-M6" % "0.11.0"
)
