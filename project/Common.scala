import sbt._, Keys._
import sbtrelease._
import sbtrelease.ReleasePlugin.autoImport._
import ReleaseStateTransformations._
import com.typesafe.sbt.pgp.PgpKeys
import xerial.sbt.Sonatype.SonatypeKeys

object Common {

  val tagName = Def.setting{
    s"v${if (releaseUseGlobalVersion.value) (version in ThisBuild).value else version.value}"
  }
  val tagOrHash = Def.setting{
    if(isSnapshot.value) gitHash() else tagName.value
  }

  private[this] def gitHash(): String =
    sys.process.Process("git rev-parse HEAD").lines_!.head

  private[this] val unusedWarnings = (
    "-Ywarn-unused" ::
    "-Ywarn-unused-import" ::
    Nil
  )

  private[this] val Scala211 = "2.11.11"

  private[this] val SetScala211 = releaseStepCommand("++" + Scala211)

  def stripPom(filter: scala.xml.Node => Boolean): Setting[_] =
    pomPostProcess := { node =>
      import scala.xml._
      import scala.xml.transform._
      val rule = new RewriteRule {
        override def transform(n: Node) =
          if (filter(n)) NodeSeq.Empty else n
      }
      new RuleTransformer(rule).transform(node)(0)
    }

  val commonSettings = scalaprops.ScalapropsPlugin.autoImport.scalapropsCoreSettings ++ Seq(
    unmanagedResources in Compile += (baseDirectory in LocalRootProject).value / "LICENSE.txt",
    resolvers += Opts.resolver.sonatypeReleases,
    publishTo := Some(
      if (isSnapshot.value)
        Opts.resolver.sonatypeSnapshots
      else
        Opts.resolver.sonatypeStaging
    ),
    scalaVersion := Scala211,
    crossScalaVersions := "2.12.3" :: Scala211 :: "2.10.6" :: "2.13.0-M2" :: Nil,
    organization := "com.github.scalaprops",
    description := "property based testing library for Scala",
    fullResolvers ~= {_.filterNot(_.name == "jcenter")},
    homepage := Some(url("https://github.com/scalaprops/scalaprops")),
    licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php")),
    commands += Command.command("updateReadme")(UpdateReadme.updateReadmeTask),
    stripPom { node =>
      node.label == "dependency" && (node \ "scope").text == "test"
    },
    scalacOptions in (Compile, doc) ++= {
      val tag = tagOrHash.value
      Seq(
        "-sourcepath", (baseDirectory in LocalRootProject).value.getAbsolutePath,
        "-doc-source-url", s"https://github.com/scalaprops/scalaprops/tree/${tag}€{FILE_PATH}.scala"
      )
    },
    pomExtra := (
      <developers>
        <developer>
          <id>xuwei-k</id>
          <name>Kenji Yoshida</name>
          <url>https://github.com/xuwei-k</url>
        </developer>
      </developers>
      <scm>
        <url>git@github.com:scalaprops/scalaprops.git</url>
        <connection>scm:git:git@github.com:scalaprops/scalaprops.git</connection>
        <tag>{tagOrHash.value}</tag>
      </scm>
    ),
    scalacOptions ++= (
      "-deprecation" ::
      "-unchecked" ::
      "-Xlint" ::
      "-Xfuture" ::
      "-language:existentials" ::
      "-language:higherKinds" ::
      "-language:implicitConversions" ::
      "-Yno-adapted-args" ::
      Nil
    ),
    scalacOptions ++= PartialFunction.condOpt(CrossVersion.partialVersion(scalaVersion.value)){
      case Some((2, v)) if v >= 11 => unusedWarnings
    }.toList.flatten,
    releaseTagName := tagName.value,
    releaseCrossBuild := true,
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runTest,
      SetScala211,
      releaseStepCommand("scalapropsNative/test"),
      setReleaseVersion,
      commitReleaseVersion,
      UpdateReadme.updateReadmeProcess,
      tagRelease,
      ReleaseStep(
        action = { state =>
          val extracted = Project extract state
          extracted.runAggregated(PgpKeys.publishSigned in Global in extracted.get(thisProjectRef), state)
        },
        enableCrossBuild = true
      ),
      SetScala211,
      releaseStepCommand("rootNative/publishSigned"),
      setNextVersion,
      commitNextVersion,
      ReleaseStep(action = Command.process("sonatypeReleaseAll", _), enableCrossBuild = true),
      UpdateReadme.updateReadmeProcess,
      pushChanges
    ),
    credentials ++= PartialFunction.condOpt(sys.env.get("SONATYPE_USER") -> sys.env.get("SONATYPE_PASSWORD")){
      case (Some(user), Some(pass)) =>
        Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, pass)
    }.toList
  ) ++ Seq(Compile, Test).flatMap(c =>
    scalacOptions in (c, console) --= unusedWarnings
  )

}
