val dottyVersion = "3.0.0-M1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "lightarrow",
    version := "0.1.0",
    scalaVersion := dottyVersion,
    libraryDependencies += "org.scalatest" % "scalatest-funsuite_3.0.0-M1" % "3.2.3"
  )
