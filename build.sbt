import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.tobiatesan",
      scalaVersion := "2.11.1",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Twist",
    libraryDependencies += scalaTest % Test
  )
