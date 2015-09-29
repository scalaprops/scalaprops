import sbt._, Keys._
import sbtrelease._
import sbtrelease.ReleasePlugin.autoImport._
import ReleaseStateTransformations._
import com.typesafe.sbt.pgp.PgpKeys
import xerial.sbt.Sonatype.SonatypeKeys

object Common {

  def shapelessDependency(scope: String) =
    libraryDependencies ++= {
      val v = build.shapelessVersion.value
      if(scalaVersion.value.startsWith("2.10")) Seq(
        "com.chuusai" %% "shapeless" % v % scope,
        compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
      ) else if(scalaVersion.value.startsWith("2.12")) {
        Nil
      } else Seq(
        "com.chuusai" %% "shapeless" % v % scope
      )
    }

  private[this] def gitHash = scala.util.Try(
    sys.process.Process("git rev-parse HEAD").lines_!.head
  ).getOrElse("master")

  private[this] val unusedWarnings = (
    "-Ywarn-unused" ::
    "-Ywarn-unused-import" ::
    Nil
  )

  private[this] val Scala211 = "2.11.7"

  val commonSettings = scalaprops.ScalapropsPlugin.autoImport.scalapropsCoreSettings ++ Seq(
    scalaVersion := Scala211,
    crossScalaVersions := "2.12.0-M2" :: Scala211 :: "2.10.6" :: Nil,
    organization := "com.github.scalaprops",
    description := "property based testing library for Scala",
    fullResolvers ~= {_.filterNot(_.name == "jcenter")},
    homepage := Some(url("https://github.com/scalaprops/scalaprops")),
    licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php")),
    commands += Command.command("updateReadme")(UpdateReadme.updateReadmeTask),
    pomPostProcess := { node =>
      import scala.xml._
      import scala.xml.transform._
      def stripIf(f: Node => Boolean) = new RewriteRule {
        override def transform(n: Node) =
          if (f(n)) NodeSeq.Empty else n
      }
      val stripTestScope = stripIf { n => n.label == "dependency" && (n \ "scope").text == "test" }
      new RuleTransformer(stripTestScope).transform(node)(0)
    },
    scalacOptions in (Compile, doc) ++= {
      val tag = if(isSnapshot.value) gitHash else { "v" + version.value }
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
        <tag>{if(isSnapshot.value) gitHash else { "v" + version.value }}</tag>
      </scm>
    ),
    scalacOptions ++= (
      "-deprecation" ::
      "-unchecked" ::
      "-Xlint" ::
      "-language:existentials" ::
      "-language:higherKinds" ::
      "-language:implicitConversions" ::
      "-Yno-adapted-args" ::
      Nil
    ),
    scalacOptions ++= PartialFunction.condOpt(CrossVersion.partialVersion(scalaVersion.value)){
      case Some((2, v)) if v >= 11 => unusedWarnings
    }.toList.flatten,
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runTest,
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
      setNextVersion,
      commitNextVersion,
      ReleaseStep{ state =>
        val extracted = Project extract state
        extracted.runAggregated(SonatypeKeys.sonatypeReleaseAll in Global in extracted.get(thisProjectRef), state)
      },
      UpdateReadme.updateReadmeProcess,
      pushChanges
    ),
    credentials ++= PartialFunction.condOpt(sys.env.get("SONATYPE_USER") -> sys.env.get("SONATYPE_PASSWORD")){
      case (Some(user), Some(pass)) =>
        Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, pass)
    }.toList
  ) ++ Seq(Compile, Test).flatMap(c =>
    scalacOptions in (c, console) ~= {_.filterNot(unusedWarnings.toSet)}
  )

}
