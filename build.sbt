Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion := "3.0.2"
ThisBuild / scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-feature",
  "-deprecation"
)

// Scalafix config:
ThisBuild / scalafixScalaBinaryVersion := "2.13"
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.5.0"
ThisBuild / scalafixDependencies += "com.github.vovapolu" %% "scaluzzi" % "0.1.20"

lazy val unref = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "unref",
    console / initialCommands := """
      |import codes.quine.labo.unref._
      |""".stripMargin,
    Compile / console / scalacOptions -= "-Wunused",
    Test / console / scalacOptions -= "-Wunused",
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
    // Settings for test:
    libraryDependencies += "org.scalameta" %%% "munit" % "1.0.0-M1" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
