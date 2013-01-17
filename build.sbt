organization := "tonymorris"

name := "course"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.2"

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test" / "src")

libraryDependencies ++= Seq( 
  "org.specs2" %% "specs2" % "1.12.3" % "test",
  "org.scalaz" % "scalaz-core_2.9.2" % "7.0.0-M7",
  "org.scalacheck" %% "scalacheck" % "1.10.0",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "7.0.0-M7"
)
