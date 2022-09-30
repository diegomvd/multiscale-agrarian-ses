ThisBuild / version := "0.1.0"
ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "multiscale-agrarian-ses",
    //idePackagePrefix := Some("MultiScaleAgrarianSES"),
    libraryDependencies += "org.scala-lang" %% "scala3-library" % "3.1.2",
    libraryDependencies += "org.scala-graph" %% "graph-core" % "1.13.5"
  )
