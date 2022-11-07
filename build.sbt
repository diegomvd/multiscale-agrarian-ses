ThisBuild / version := "1.0.0"
ThisBuild / scalaVersion := "3.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "multiscale-agrarian-ses",
    libraryDependencies += "org.scala-lang" %% "scala3-library" % "3.1.2",
    libraryDependencies += "org.scala-graph" % "graph-core_2.13" % "1.13.5"
  )

enablePlugins(SbtOsgi)

OsgiKeys.exportPackage := Seq("MultiScaleAgrarianSES.*")

OsgiKeys.importPackage := Seq("*;resolution:=optional")

OsgiKeys.privatePackage := Seq("!scala.*","*")

OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))""""