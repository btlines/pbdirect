name := "pbdirect"

version := "0.1.0"

scalaVersion := "2.12.2"

crossScalaVersions := Seq("2.11.11", "2.12.4")

libraryDependencies ++= Seq(
  "com.chuusai"         %% "shapeless"     % "2.3.3",
  "org.typelevel"       %% "cats-core"     % "1.0.1",
  "com.google.protobuf" %  "protobuf-java" % "3.5.1",
  "org.scalatest"       %% "scalatest"     % "3.0.4" % Test
)

organization := "beyondthelines"

licenses := ("MIT", url("http://opensource.org/licenses/MIT")) :: Nil

bintrayOrganization := Some("beyondthelines")

bintrayPackageLabels := Seq("scala", "protobuf")

