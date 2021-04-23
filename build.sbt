lazy val scala212 = "2.12.13"
lazy val scala213 = "2.13.5"
lazy val supportedScalaVersions = List(scala212, scala213)

ThisBuild / organization := "net.reactivecore"
ThisBuild / version := "0.3.0"
ThisBuild / scalaVersion := scala212
ThisBuild / scalacOptions += "-feature"
ThisBuild / scalacOptions += "-deprecation"

val akkaVersion = "2.6.14"
val akkaHttpVersion = "10.2.4"
val scalaTestVersion = "3.0.9"
val circeVersion = "0.13.0"
val shapelessVersion = "2.3.4"
val macroParadiseVersion = "2.1.1"

// Releasing settings
ThisBuild / publishTo := sonatypePublishTo.value
// ThisBuild / publishMavenStyle := true
ThisBuild / licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url("https://github.com/reactivecore/fhttp"))
ThisBuild / developers := List(
  Developer(id="nob13", name="Norbert Schultz", email="norbert.schultz@reactivecore.de", url=url("https://www.reactivecore.de"))
)

usePgpKeyHex("77D0E9E04837F8CBBCD56429897A43978251C225")

import scalariform.formatter.preferences._
val scalariformSettings = {
  scalariformPreferences := scalariformPreferences.value
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(DoubleIndentConstructorArguments, true)
    .setPreference(DanglingCloseParenthesis, Preserve)
}

/** Returns yes if Scala version is < 2.13, no otherwise */
def priorTo213[T](scalaVersion: String)(yes: T*)(no: T*): Seq[T] = {
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, minor)) if minor < 13 => yes
    case _                              => no
  }
}

lazy val compilerDependentSettings: Seq[Setting[_]] = Seq(
  libraryDependencies ++=
    Seq(
      scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided,
      scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided
    ) ++ priorTo213(scalaVersion.value) (compilerPlugin(("org.scalamacros" % "paradise" % macroParadiseVersion).cross(CrossVersion.patch)))(),
  scalacOptions ++= priorTo213(scalaVersion.value)(
    "-Ypartial-unification"
  )(
    "-Ymacro-annotations"
  )
)

lazy val core = (project in file("lib"))
  .settings(
    name := "fhttp-core",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
      "com.chuusai" %% "shapeless" % shapelessVersion,

      // For JSON
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion
    ),
    scalariformSettings,
    crossScalaVersions := supportedScalaVersions
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
      crossScalaVersions := supportedScalaVersions
    )
  .settings(compilerDependentSettings)

lazy val example = (project in file("example"))
  .dependsOn(core, akka)
  .settings(
    name := "fhttp-example",
    scalariformSettings,
    publish := {},
    publishLocal := {},
    publishArtifact := false,
    crossScalaVersions := Nil
  )
  .settings(compilerDependentSettings)

lazy val root = (project in file("."))
  .aggregate(core, akka, example)
  .settings(
    name := "fhttp-root",
    publish := {},
    publishLocal := {},
    publishArtifact := false,
    test := {},
    crossScalaVersions := Nil
  )
  .settings(compilerDependentSettings)
