import sbtcrossproject.CrossPlugin.autoImport.crossProject

val pbdirect = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(
    name := "pbdirect",
    version := "0.0.10",
    scalaVersion := "2.12.8",
    crossScalaVersions := Seq("2.11.12", "2.12.8"),
    libraryDependencies ++= Seq(
      "com.chuusai"         %%% "shapeless"     % "2.3.3",
      "org.typelevel"       %%% "cats-core"     % "1.6.0",
      "org.scalatest"       %%% "scalatest"     % "3.0.7" % Test
    ),
    organization := "beyondthelines",
    licenses := ("MIT", url("http://opensource.org/licenses/MIT")) :: Nil,
    bintrayOrganization := Some("beyondthelines"),
    bintrayPackageLabels := Seq("scala", "protobuf"),
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "com.google.protobuf" % "protobuf-java" % "3.8.0"
    )
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time" % "2.0.0-RC2" % "test",
      "com.thesamet.scalapb" %%% "protobuf-runtime-scala" % "0.8.1"
    )
  )
