name := """arguments-parent"""

version in ThisBuild := "0.1-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.2"

lazy val lib = {
  project.in(file("lib")).
    settings(
      name := "arguments-lib",
      libraryDependencies ++= List(
        "org.scala-lang" % "scala-reflect" % "2.11.2"))
}

lazy val api = {
  project.in(file("api")).
    dependsOn(lib).
    settings(
      name := "arguments",
      libraryDependencies ++= List(
        "com.github.scopt" %% "scopt" % "3.2.0",
        "org.scalatest" %% "scalatest" % "2.2.2" % "test"))
}

lazy val root = project.in(file(".")).dependsOn(api, lib).aggregate(api, lib)
