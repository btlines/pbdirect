name := "pbdirect"

version := "0.0.3"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  "com.chuusai"         %% "shapeless"     % "2.3.2",
  "org.typelevel"       %% "cats"          % "0.9.0",
  "com.google.protobuf" %  "protobuf-java" % "3.2.0",
  "org.scalatest"       %% "scalatest"     % "3.0.1" % Test
)

organization := "beyondthelines"

licenses := ("MIT", url("http://opensource.org/licenses/MIT")) :: Nil

bintrayOrganization := Some("beyondthelines")

bintrayPackageLabels := Seq("scala", "protobuf")
