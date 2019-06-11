name := "pbdirect"

version := "0.0.10"

scalaVersion := "2.12.8"

crossScalaVersions := Seq("2.11.12", "2.12.8")

libraryDependencies ++= Seq(
  "com.chuusai"         %% "shapeless"     % "2.3.3",
  "org.typelevel"       %% "cats-core"     % "1.6.1",
  "com.google.protobuf" %  "protobuf-java" % "3.8.0",
  "org.scalatest"       %% "scalatest"     % "3.0.8" % Test
)

organization := "beyondthelines"

licenses := ("MIT", url("http://opensource.org/licenses/MIT")) :: Nil

bintrayOrganization := Some("beyondthelines")

bintrayPackageLabels := Seq("scala", "protobuf")

