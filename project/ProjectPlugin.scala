import de.heikoseeberger.sbtheader.HeaderPlugin.autoImport.{headerLicense, HeaderLicense}
import sbt.Keys._
import sbt._
import sbtorgpolicies.OrgPoliciesPlugin
import sbtorgpolicies.OrgPoliciesPlugin.autoImport._
import sbtorgpolicies.model._
import sbtorgpolicies.templates._
import sbtorgpolicies.templates.badges._
import sbtorgpolicies.runnable.syntax._
import scala.language.reflectiveCalls

object ProjectPlugin extends AutoPlugin {

  override def requires: Plugins = OrgPoliciesPlugin

  override def trigger: PluginTrigger = allRequirements

  object autoImport {

    lazy val V = new {
      val cats: String      = "2.0.0"
      val protobuf: String  = "3.11.1"
      val scala211: String  = "2.11.12"
      val scala212: String  = "2.12.10"
      val scalaTest: String = "3.1.0"
      val shapeless: String = "2.3.3"
    }
  }

  import autoImport._

  case class FixedCodecovBadge(info: BadgeInformation) extends Badge(info) {

    override def badgeIcon: Option[BadgeIcon] =
      BadgeIcon(
        title = "codecov.io",
        icon = s"http://codecov.io/gh/${info.owner}/${info.repo}/branch/master/graph/badge.svg",
        url = s"http://codecov.io/gh/${info.owner}/${info.repo}"
      ).some
  }

  override def projectSettings: Seq[Def.Setting[_]] =
    sharedReleaseProcess ++ warnUnusedImport ++ Seq(
      orgProjectName := "pbdirect",
      orgGithubSetting := GitHubSettings(
        organization = "47deg",
        project = (name in LocalRootProject).value,
        organizationName = "47 Degrees",
        groupId = "com.47deg",
        organizationHomePage = url("http://47deg.com"),
        organizationEmail = "hello@47deg.com"
      ),
      orgLicenseSetting := MITLicense,
      headerLicense := Some(
        HeaderLicense.MIT(replaceableYear(startYear.value), "Beyond the lines")),
      startYear := Some(2019),
      scalaVersion := V.scala212,
      crossScalaVersions := Seq(scalaVersion.value, V.scala211),
      libraryDependencies ++= Seq(
        "com.chuusai"         %% "shapeless"    % V.shapeless,
        "org.typelevel"       %% "cats-core"    % V.cats,
        "com.google.protobuf" % "protobuf-java" % V.protobuf,
        "org.scalatest"       %% "scalatest"    % V.scalaTest % Test
      ),
      orgScriptTaskListSetting := List(
        (clean in Global).asRunnableItemFull,
        (compile in Compile).asRunnableItemFull,
        (test in Test).asRunnableItemFull
      ),
      orgMaintainersSetting := List(
        Dev("developer47deg", Some("47 Degrees (twitter: @47deg)"), Some("hello@47deg.com"))),
      // format: OFF
      orgBadgeListSetting := List(
        TravisBadge.apply,
        CodecovBadge.apply, { info => MavenCentralBadge.apply(info.copy(libName = "pbdirect")) },
        ScalaLangBadge.apply,
        LicenseBadge.apply, { info => GitterBadge.apply(info.copy(owner = "47deg", repo = "pbdirect")) },
        GitHubIssuesBadge.apply
      ),
      orgEnforcedFilesSetting := List(
        ContributingFileType(orgProjectName.value, orgGithubSetting.value.copy(organization = "47deg", project = "pbdirect")),
        AuthorsFileType(name.value, orgGithubSetting.value, orgMaintainersSetting.value, orgContributorsSetting.value),
        NoticeFileType(orgProjectName.value, orgGithubSetting.value, orgLicenseSetting.value, startYear.value),
        VersionSbtFileType,
        ChangelogFileType,
        ReadmeFileType(orgProjectName.value, orgGithubSetting.value, startYear.value, orgLicenseSetting.value, orgCommitBranchSetting.value, sbtPlugin.value, name.value, version.value, scalaBinaryVersion.value, sbtBinaryVersion.value, orgSupportedScalaJSVersion.value, orgBadgeListSetting.value),
        ScalafmtFileType,
        TravisFileType(crossScalaVersions.value, orgScriptCICommandKey, orgAfterCISuccessCommandKey)
        // format: ON
      )
    )
}
