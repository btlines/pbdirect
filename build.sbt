ThisBuild / scalaVersion := "2.13.1"
ThisBuild / crossScalaVersions := Seq("2.12.11", "2.13.1")
ThisBuild / organization := "com.47deg"

addCommandAlias(
  "ci-test",
  "+scalafmtCheckAll; +scalafmtSbtCheck; project-docs/mdoc; +coverage; +test; +coverageReport; +coverageAggregate"
)
addCommandAlias("ci-docs", "project-docs/mdoc; headerCreateAll")

lazy val pbdirect = project
  .in(file("."))
  .settings(name := "pbdirect")

lazy val `project-docs` = (project in file(".docs"))
  .dependsOn(pbdirect)
  .settings(moduleName := "pbdirect-project-docs")
  .settings(mdocIn := file(".docs"))
  .settings(mdocOut := file("."))
  .settings(skip in publish := true)
  .enablePlugins(MdocPlugin)
