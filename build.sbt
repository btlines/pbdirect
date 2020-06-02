ThisBuild / scalaVersion := "2.13.1"
ThisBuild / crossScalaVersions := Seq("2.12.11", "2.13.1")
ThisBuild / organization := "com.47deg"

addCommandAlias(
  "ci-test",
  "+scalafmtCheckAll; +scalafmtSbtCheck; documentation/mdoc; +coverage; +test; +coverageReport; +coverageAggregate"
)
addCommandAlias("ci-docs", "documentation/mdoc; headerCreateAll")

lazy val pbdirect = project

lazy val documentation = project
  .dependsOn(pbdirect)
  .settings(skip in publish := true)
  .enablePlugins(MdocPlugin)
  .settings(mdocOut := file("."))
