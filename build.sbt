import Dependencies._

enablePlugins(ScalaJSPlugin)

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.7",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "knave",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.0" % "test"
  )

scalaJSUseMainModuleInitializer := true