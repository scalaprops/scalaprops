import build._
import sbtrelease._
import ReleaseStateTransformations._
import com.typesafe.sbt.pgp.PgpKeys
import sbtcrossproject.CrossProject

val scalazVersion = SettingKey[String]("scalazVersion")

// avoid move files
val CustomCrossType = new sbtcrossproject.CrossType {
  override def projectDir(crossBase: File, projectType: String) =
    crossBase / projectType

  override def projectDir(crossBase: File, projectType: sbtcrossproject.Platform) = {
    val dir = projectType match {
      case JVMPlatform => "jvm"
      case JSPlatform => "js"
      case NativePlatform => "native"
    }
    crossBase / dir
  }

  def shared(projectBase: File, conf: String) =
    projectBase.getParentFile / "src" / conf / "scala"

  override def sharedSrcDir(projectBase: File, conf: String) =
    Some(shared(projectBase, conf))
}

def module(id: String): CrossProject =
  CrossProject(id, file(id))(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CustomCrossType)
    .settings(
      commonSettings,
      scalazVersion := "7.2.26",
      initialCommands in console += {
        "import scalaprops._, scalaz._;" + Seq(
          "Gen",
          "Cogen",
          "Rand"
        ).map(a => s"val $a = scalaprops.$a").mkString(";") // for tab completion
      }
    )
    .jsSettings(
      scalacOptions += {
        val a = (baseDirectory in LocalRootProject).value.toURI.toString
        val g = "https://raw.githubusercontent.com/scalaprops/scalaprops/" + tagOrHash.value
        s"-P:scalajs:mapSourceURI:$a->$g/"
      }
    )
    .nativeSettings(
      nativeGC := "immix"
    )

lazy val gen = module("gen")
  .settings(
    name := genName,
    description := "pure functional random value generator",
    libraryDependencies += "org.scalaz" %%% "scalaz-core" % scalazVersion.value
  )
  .platformsSettings(JSPlatform, NativePlatform)(
    unmanagedSourceDirectories in Compile += {
      baseDirectory.value.getParentFile / "js_native/src/main/scala/"
    }
  )
  .jvmSettings(
    Generator.settings
  )

lazy val core = module("core")
  .settings(
    name := coreName
  )
  .dependsOn(gen)

lazy val scalazlaws = module("scalazlaws")
  .settings(
    name := scalazlawsName
  )
  .dependsOn(core)

lazy val scalaprops = module(scalapropsName)
  .settings(
    scalacOptions := {
      // suppress some warnings if Scala 2.12
      // see https://github.com/scala/scala/pull/5402 and "scala -Ywarn-unused:help"
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v >= 12 =>
          scalacOptions.value.filterNot(_ == "-Ywarn-unused") :+ "-Ywarn-unused:-params,-patvars,_"
        case _ =>
          scalacOptions.value
      }
    },
    name := scalapropsName
  )
  .dependsOn(
    core,
    scalazlaws % "test"
  )
  .jvmSettings(
    libraryDependencies += "org.scala-sbt" % "test-interface" % "1.0"
  )
  .platformsSettings(JVMPlatform, NativePlatform)(
    unmanagedSourceDirectories in Compile += {
      baseDirectory.value.getParentFile / "jvm_native/src/main/scala/"
    }
  )
  .jsSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-test-interface" % scalaJSVersion
  )
  .nativeSettings(
    libraryDependencies += "org.scala-native" %%% "test-interface" % nativeVersion
  )

val tagName = Def.setting {
  s"v${if (releaseUseGlobalVersion.value) (version in ThisBuild).value
  else version.value}"
}
val tagOrHash = Def.setting {
  if (isSnapshot.value) gitHash() else tagName.value
}

def gitHash(): String =
  sys.process.Process("git rev-parse HEAD").lines_!.head

val unusedWarnings = Seq("-Ywarn-unused")

val Scala211 = "2.11.12"
val SetScala211 = releaseStepCommand("++" + Scala211)

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

val commonSettings = _root_.scalaprops.ScalapropsPlugin.autoImport.scalapropsCoreSettings ++ Seq(
  unmanagedResources in Compile += (baseDirectory in LocalRootProject).value / "LICENSE.txt",
  resolvers += Opts.resolver.sonatypeReleases,
  publishTo := Some(
    if (isSnapshot.value)
      Opts.resolver.sonatypeSnapshots
    else
      Opts.resolver.sonatypeStaging
  ),
  scalaVersion := Scala211,
  crossScalaVersions := "2.12.6" :: Scala211 :: "2.10.7" :: "2.13.0-M4" :: "2.13.0-M5" :: Nil,
  organization := "com.github.scalaprops",
  description := "property based testing library for Scala",
  fullResolvers ~= { _.filterNot(_.name == "jcenter") },
  homepage := Some(url("https://github.com/scalaprops/scalaprops")),
  licenses := Seq("MIT License" -> url("https://opensource.org/licenses/mit-license")),
  commands += Command.command("updateReadme")(UpdateReadme.updateReadmeTask),
  stripPom { node =>
    node.label == "dependency" && (node \ "scope").text == "test"
  },
  scalacOptions in (Compile, doc) ++= {
    val tag = tagOrHash.value
    Seq(
      "-sourcepath",
      (baseDirectory in LocalRootProject).value.getAbsolutePath,
      "-doc-source-url",
      s"https://github.com/scalaprops/scalaprops/tree/${tag}€{FILE_PATH}.scala"
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
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-Xfuture",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions"
  ),
  scalacOptions ++= PartialFunction
    .condOpt(CrossVersion.partialVersion(scalaVersion.value)) {
      case Some((2, v)) if v <= 12 => "-Yno-adapted-args"
    }
    .toList,
  scalacOptions ++= PartialFunction
    .condOpt(CrossVersion.partialVersion(scalaVersion.value)) {
      case Some((2, v)) if v >= 11 => unusedWarnings
    }
    .toList
    .flatten,
  releaseTagName := tagName.value,
  releaseCrossBuild := true,
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
    SetScala211,
    releaseStepCommand("rootNative/publishSigned"),
    setNextVersion,
    commitNextVersion,
    releaseStepCommand("sonatypeReleaseAll"),
    UpdateReadme.updateReadmeProcess,
    pushChanges
  ),
  credentials ++= PartialFunction
    .condOpt(sys.env.get("SONATYPE_USER") -> sys.env.get("SONATYPE_PASSWORD")) {
      case (Some(user), Some(pass)) =>
        Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, pass)
    }
    .toList
) ++ Seq(Compile, Test).flatMap(c => scalacOptions in (c, console) --= unusedWarnings)

lazy val notPublish = Seq(
  publishArtifact := false,
  publish := {},
  publishLocal := {},
  PgpKeys.publishSigned := {},
  PgpKeys.publishLocalSigned := {}
)

lazy val jvmProjects = Seq[ProjectReference](
  genJVM,
  coreJVM,
  scalapropsJVM,
  scalazlawsJVM
)

lazy val jsProjects = Seq[ProjectReference](
  genJS,
  coreJS,
  scalapropsJS,
  scalazlawsJS
)

lazy val nativeProjects = Seq[ProjectReference](
  genNative,
  coreNative,
  scalapropsNative,
  scalazlawsNative
)

lazy val genJS = gen.js
lazy val genJVM = gen.jvm
lazy val genNative = gen.native
lazy val genRoot = project
  .aggregate(genJS, genJVM, genNative)
  .settings(
    commonSettings,
    notPublish
  )

lazy val coreJS = core.js
lazy val coreJVM = core.jvm
lazy val coreNative = core.native
lazy val coreRoot = project
  .aggregate(coreJS, coreJVM, genNative)
  .settings(
    commonSettings,
    notPublish
  )

lazy val scalazlawsJS = scalazlaws.js
lazy val scalazlawsJVM = scalazlaws.jvm
lazy val scalazlawsNative = scalazlaws.native
lazy val scalazlawsRoot = project
  .aggregate(scalazlawsJS, scalazlawsJVM, scalazlawsNative)
  .settings(
    commonSettings,
    notPublish
  )

lazy val scalapropsJS = scalaprops.js
lazy val scalapropsJVM = scalaprops.jvm
lazy val scalapropsNative = scalaprops.native.settings(
  scalapropsNativeSettings
)
lazy val scalapropsRoot = project
  .aggregate(scalapropsJS, scalapropsJVM, scalapropsNative)
  .settings(
    commonSettings,
    notPublish
  )

val root = Project("root", file("."))
  .settings(
    commonSettings,
    (
      coreJVM ::
        scalapropsJVM ::
        scalazlawsJVM ::
        Nil
    ).map(p => libraryDependencies ++= (libraryDependencies in p).value)
  )
  .enablePlugins(
    ScalaUnidocPlugin
  )
  .settings(
    name := allName,
    artifacts := Nil,
    unidocProjectFilter in (ScalaUnidoc, unidoc) := {
      (jsProjects ++ nativeProjects).foldLeft(inAnyProject)((acc, a) => acc -- inProjects(a))
    },
    packagedArtifacts := Map.empty,
    artifacts ++= Classpaths.artifactDefs(Seq(packageDoc in Compile)).value,
    packagedArtifacts ++= Classpaths.packaged(Seq(packageDoc in Compile)).value,
    description := "scalaprops unidoc",
    stripPom { _.label == "dependencies" },
    Sxr.settings1,
    Defaults.packageTaskSettings(
      packageDoc in Compile,
      (unidoc in Compile).map { _.flatMap(Path.allSubpaths) }
    ),
    Sxr.settings2
  )
  .aggregate(
    jvmProjects ++ jsProjects: _* // ignore native
  )

lazy val rootJS = project
  .aggregate(jsProjects: _*)
  .settings(
    commonSettings,
    notPublish
  )
lazy val rootJVM = project
  .aggregate(jvmProjects: _*)
  .settings(
    commonSettings,
    notPublish
  )
lazy val rootNative = project
  .aggregate(nativeProjects: _*)
  .settings(
    commonSettings,
    notPublish
  )
