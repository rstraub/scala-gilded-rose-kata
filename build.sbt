import Dependencies.*
import sbt.Keys.libraryDependencies

def itFilter(name: String): Boolean = name.endsWith("IT")

lazy val root = project
  .enablePlugins(ScalafmtPlugin)
  .in(file("."))
  .settings(
    name := "Scala Gilded Rose",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := projectScalaVersion,
    libraryDependencies += scalaTest % "it,test",
    libraryDependencies += mockito % Test
  )

val projectScalaVersion = "2.13.11"
