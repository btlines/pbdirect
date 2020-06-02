import sbt.Keys._
import sbt._
import scala.language.reflectiveCalls
import com.alejandrohdezma.sbt.github.SbtGithubPlugin

object ProjectPlugin extends AutoPlugin {

  override def requires: Plugins = SbtGithubPlugin

  override def trigger: PluginTrigger = allRequirements

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      libraryDependencies ++= Seq(
        "com.chuusai"                %% "shapeless"                 % "2.3.3",
        "org.typelevel"              %% "cats-core"                 % "2.1.1",
        "com.google.protobuf"         % "protobuf-java"             % "3.12.2",
        "com.beachape"               %% "enumeratum"                % "1.6.1",
        "org.scalatest"              %% "scalatest"                 % "3.1.2" % Test,
        "org.scalatestplus"          %% "scalacheck-1-14"           % "3.1.2.0" % Test,
        "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.5" % Test,
        "com.github.os72"             % "protoc-jar"                % "3.11.4" % Test
      ),
      Compile / scalacOptions -= "-Xfatal-warnings",
      Compile / console / scalacOptions -= "-Xlint"
    )
}
