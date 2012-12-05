organization := "tonymorris"

name := "course"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.2"

scalaSource in Compile <<= baseDirectory(_ / "src")

libraryDependencies ++= Seq( 
  "org.scalatest" %% "scalatest" % "1.6.1" % "test",
  "org.scalaz" % "scalaz-core_2.9.2" % "7.0.0-M3",
  "org.scalacheck" %% "scalacheck" % "1.10.0",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "7.0.0-M3"
)