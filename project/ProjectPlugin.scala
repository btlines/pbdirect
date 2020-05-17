import de.heikoseeberger.sbtheader.HeaderPlugin.autoImport._
import de.heikoseeberger.sbtheader.CommentStyle
import sbt.Keys._
import sbt._
import scala.language.reflectiveCalls
import com.alejandrohdezma.sbt.github.SbtGithubPlugin

object ProjectPlugin extends AutoPlugin {

  override def requires: Plugins = SbtGithubPlugin

  override def trigger: PluginTrigger = allRequirements

  object autoImport {

    lazy val V = new {
      val cats: String                = "2.1.1"
      val protobuf: String            = "3.12.0"
      val scala212: String            = "2.12.11"
      val scala213: String            = "2.13.1"
      val shapeless: String           = "2.3.3"
      val enumeratum: String          = "1.6.1"
      val scalaTest: String           = "3.1.2"
      val scalatestScalacheck: String = "3.1.2.0"
      val scalacheckShapeless: String = "1.2.5"
      val protocJar: String           = "3.11.4"
    }
  }

  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      organization := "com.47deg",
      crossScalaVersions := Seq(V.scala212, V.scala213),
      // disable license headers on source files because it's too complicated, owing to us forking the project
      headerLicense := Some(HeaderLicense.Custom("")),
      headerMappings := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.cppStyleLineComment),
      crossScalaVersions := Seq(scalaVersion.value, V.scala213),
      libraryDependencies ++= Seq(
        "com.chuusai"                %% "shapeless"                 % V.shapeless,
        "org.typelevel"              %% "cats-core"                 % V.cats,
        "com.google.protobuf"         % "protobuf-java"             % V.protobuf,
        "com.beachape"               %% "enumeratum"                % V.enumeratum,
        "org.scalatest"              %% "scalatest"                 % V.scalaTest           % Test,
        "org.scalatestplus"          %% "scalacheck-1-14"           % V.scalatestScalacheck % Test,
        "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % V.scalacheckShapeless % Test,
        "com.github.os72"             % "protoc-jar"                % V.protocJar           % Test
      ),
      Compile / scalacOptions -= "-Xfatal-warnings",
      Compile / console / scalacOptions -= "-Xlint"
    )
}
