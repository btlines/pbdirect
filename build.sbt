import sbtcrossproject.CrossPlugin.autoImport.crossProject
import sbtcrossproject.CrossPlugin.autoImport.CrossType.Full

licenses in ThisBuild := ("MIT", url("http://opensource.org/licenses/MIT")) :: Nil
organization in ThisBuild := "beyondthelines"
bintrayOrganization in ThisBuild := Some("beyondthelines")
bintrayPackageLabels in ThisBuild := Seq("scala", "protobuf")

val pbdirect = crossProject(JSPlatform, JVMPlatform)
  .crossType(Full)
  .in(file("."))
  .enablePlugins(GitVersioning)
  .settings(
    name := "pbdirect",
    scalaVersion := "2.11.12",
    crossScalaVersions := Seq("2.11.12", "2.12.8", "2.13.0"),
    libraryDependencies ++= Seq(
      "com.chuusai"       %%% "shapeless"  % "2.3.3",
      "org.typelevel"     %%% "cats-core"  % "2.0.0",
      "org.scalatest"     %%% "scalatest"  % "3.0.8" % Test
    ),
    resolvers += "Sonatype OSS Snapshots" at
    "https://oss.sonatype.org/content/repositories/releases",
    git.useGitDescribe := true
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "com.google.protobuf" % "protobuf-java"   % "3.9.2"
    )
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      "io.github.cquiroz"    %%% "scala-java-time"        % "2.0.0-RC3" % "test",
      "com.thesamet.scalapb" %%% "protobuf-runtime-scala" % "0.8.2"
    )
  )

addCommandAlias("fmt", ";scalafmt;test:scalafmt;scalafmtSbt")
