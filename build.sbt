ThisBuild / version := "1.0.0"
ThisBuild / scalaVersion := "3.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "multiscale-agrarian-ses",
    libraryDependencies += "org.scala-lang" %% "scala3-library" % "3.1.2",
    //libraryDependencies += "org.scala-graph" % "graph-core_2.13" % "1.13.5",
    // https://mvnrepository.com/artifact/org.jgrapht/jgrapht-core
    libraryDependencies += "org.jgrapht" % "jgrapht-core" % "1.5.1"
  )

enablePlugins(SbtOsgi)

OsgiKeys.exportPackage := Seq("MultiScaleAgrarianSES.*","MultiScaleConservationPlanning.*")

OsgiKeys.importPackage := Seq("*;resolution:=optional")

OsgiKeys.privatePackage := Seq("!scala.*","*")

OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))""""