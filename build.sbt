addCommandAlias(
  "ci-test",
  "+scalafmtCheck; +scalafmtSbtCheck; project-docs/mdoc; +coverage; +test; +coverageReport; +coverageAggregate"
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
