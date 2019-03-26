import sbt.project

val scalaV = "2.12.8"
val scalaTestV = "3.0.5"

lazy val mainDeps = Seq(
  "org.rogach" %% "scallop" % "3.2.0"
)

lazy val testDeps = Set(
  "org.scalatest" %% "scalatest" % scalaTestV % "test"
)

lazy val root = (project in file("."))
  .settings(
    name := "chowser",
    scalaVersion := scalaV,
    libraryDependencies ++= (mainDeps ++ testDeps)
  )
