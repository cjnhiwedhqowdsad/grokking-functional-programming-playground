ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.1"

lazy val root = (project in file("."))
  .settings(
    name := "PlaygroundScala"
  )

libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.5"
libraryDependencies += "co.fs2" %% "fs2-core" % "3.11.0"
libraryDependencies += "org.apache.jena" % "apache-jena-libs" % "5.2.0"
