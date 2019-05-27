enablePlugins(ScalaJSPlugin)

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.7",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "knave",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.5" % "test",
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.6"
  )

scalaJSUseMainModuleInitializer := true