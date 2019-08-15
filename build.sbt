import sbt.project

val chowserV = "1.7.0"
val scalaV = "2.12.8"
val scalaTestV = "3.0.5"
val betterFilesV = "3.8.0"
val scallopV = "3.2.0"
val htsjdkV = "2.19.0"

lazy val mainDeps = Seq(
  "com.github.pathikrit" %% "better-files" % betterFilesV,
  "org.rogach" %% "scallop" % scallopV,
  "com.github.samtools" % "htsjdk" % htsjdkV
)

lazy val testDeps = Set(
  "org.scalatest" %% "scalatest" % scalaTestV % "test"
)

lazy val root = (project in file("."))
  .settings(
    name := "chowser",
    version := chowserV,
    scalaVersion := scalaV,
    libraryDependencies ++= (mainDeps ++ testDeps),
    scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked"),
    mainClass := Some("chowser.app.ChowserApp"),
    maintainer := "Oliver A Ruebenacker <oliverr@broadinstitute.org>",
    packageSummary := "Command-line data munging app",
    packageDescription := "Command-line app to process genomic and other tab-separated data.",
    debianPackageDependencies := Seq("java8-runtime-headless"),
    debianNativeBuildOptions in Debian := Seq("-Zgzip", "-z3") // gzip compression at level 3
  ).enablePlugins(JavaAppPackaging, DebianPlugin)

