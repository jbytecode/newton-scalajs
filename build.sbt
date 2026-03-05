val scala3Version = "3.8.2"

lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "newton",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    scalaJSUseMainModuleInitializer := true,

    libraryDependencies ++= Seq(
      "org.scala-js"  %%% "scalajs-dom" % "2.8.1",
      "org.scalameta" %%% "munit"       % "1.0.0" % Test
    )
  )
