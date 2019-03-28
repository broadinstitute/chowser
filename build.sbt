import sbt.project

val chowserV = "0.0.1"
val scalaV = "2.12.8"
val scalaTestV = "3.0.5"

lazy val mainDeps = Seq(
  "com.github.pathikrit" %% "better-files" % "3.7.1",
  "org.rogach" %% "scallop" % "3.2.0"
)

lazy val testDeps = Set(
  "org.scalatest" %% "scalatest" % scalaTestV % "test"
)

lazy val root = (project in file("."))
  .settings(
    name := "chowser",
    version := chowserV,
    scalaVersion := scalaV,
    libraryDependencies ++= (mainDeps ++ testDeps)
  )

