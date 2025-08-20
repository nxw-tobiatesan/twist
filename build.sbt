import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.tobiatesan",
      scalaVersion := "2.12.20",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Twist",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test,
    libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.4",
    mainClass := Some("com.tobiatesan.Twist.Main")
  )

// resolvers (0.13 syntax)
resolvers ++= Seq(
  "Maven Central" at "https://repo1.maven.org/maven2/",
  Resolver.typesafeRepo("releases")
)

// (optional) pin jline to avoid the old transitive
dependencyOverrides += "jline" % "jline" % "2.14.6"
// or as a direct dependency:
// libraryDependencies += "jline" % "jline" % "2.14.6"