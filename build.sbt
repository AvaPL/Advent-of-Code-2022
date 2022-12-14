ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "Advent-of-Code-2022",
    idePackagePrefix := Some("io.github.avapl"),
    libraryDependencies ++= Seq(
      "com.softwaremill.quicklens" %% "quicklens" % "1.9.0",
      "com.typesafe.play" %% "play-json" % "2.9.3"
    )
  )
