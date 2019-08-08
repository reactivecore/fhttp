ThisBuild / organization := "net.reactivecore"
ThisBuild / version := "0.1-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.8"
// ThisBuild / scalacOptions += "-Xfatal-warnings" // this breaks the doc target due https://github.com/scala/bug/issues/10134
ThisBuild / scalacOptions += "-feature"
ThisBuild / scalacOptions += "-deprecation"
ThisBuild / scalacOptions += "-Ypartial-unification" // Needed for Cats
ThisBuild / updateOptions := updateOptions.value.withGigahorse(false) // See https://github.com/sbt/sbt/issues/3570

val akkaVersion = "2.5.20"
val akkaHttpVersion = "10.1.7"
val scalaTestVersion = "3.0.5"
val circeVersion = "0.11.1"
val shapelessVersion = "2.3.3"

import scalariform.formatter.preferences._
val scalariformSettings = {
  scalariformPreferences := scalariformPreferences.value
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(DoubleIndentConstructorArguments, true)
    .setPreference(DanglingCloseParenthesis, Preserve)
}

def paradiseSupport = addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

lazy val core = (project in file("lib"))
  .settings(
    name := "fhttp-core",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
      "com.chuusai" %% "shapeless" % shapelessVersion,

      // For JSON
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "io.circe" %% "circe-java8" % circeVersion,
    ),
    scalariformSettings
  )

lazy val akka = (project in file("akka"))
    .dependsOn(core)
    .settings(
      name := "fhttp-akka",
      libraryDependencies ++= Seq(
        "com.typesafe.akka" %% "akka-actor" % akkaVersion,
        "com.typesafe.akka" %% "akka-stream" % akkaVersion,
        "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,

        "org.scalatest" %% "scalatest" % scalaTestVersion % Test
      ),
      scalariformSettings,
      paradiseSupport
    )

lazy val example = (project in file("example"))
  .dependsOn(core, akka)
  .settings(
    name := "fhttp-example",
    scalariformSettings,
    publish := {},
    publishLocal := {},
    paradiseSupport
  )

lazy val root = (project in file("."))
  .aggregate(core, akka, example)
  .settings(
    name := "fhttp-root",
    publish := {},
    publishLocal := {},
    test := {}
  )
