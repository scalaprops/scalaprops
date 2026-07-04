import build._
import sbtrelease.ReleaseStateTransformations._

val Scala212 = "2.12.21"
val Scala213 = "2.13.18"
val Scala3 = "3.3.8"

val scalaVersions = Seq(Scala212, Scala213, Scala3)

val isScala3 = Def.setting(
  CrossVersion.partialVersion(scalaVersion.value).exists(_._1 == 3)
)

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / useSuperShell := false

lazy val jvmNative = Def.settings(
  (Compile / unmanagedSourceDirectories) += {
    (projectMatrixBaseDirectory.value / "jvm_native/src/main/scala/").getAbsoluteFile
  }
)

lazy val jsNative = Def.settings(
  Compile / unmanagedSourceDirectories += {
    (projectMatrixBaseDirectory.value / "js_native/src/main/scala").getAbsoluteFile
  },
)

def module(
  id: String,
  jvm: SettingsDefinition = Nil,
  js: SettingsDefinition = Nil,
  native: SettingsDefinition = Nil
): ProjectMatrix =
  ProjectMatrix(id, file(id), this.getClass.getClassLoader)
    .defaultAxes()
    .settings(
      commonSettings,
      Seq(Compile, Test).map { c =>
        c / unmanagedSourceDirectories ++= {
          val base = projectMatrixBaseDirectory.value / "src" / Defaults.nameForSrc(c.name)
          CrossVersion.partialVersion(scalaVersion.value) match {
            case Some((v, _)) =>
              Seq((base / s"scala-${v}").getAbsoluteFile)
            case _ =>
              Nil
          }
        }
      },
      Seq(Compile, Test).map { c =>
        (c / unmanagedSourceDirectories) += {
          val base = projectMatrixBaseDirectory.value / "src" / Defaults.nameForSrc(c.name)
          val dir = CrossVersion.partialVersion(scalaVersion.value) match {
            case Some((2, v)) if v <= 12 =>
              "scala-2.13-"
            case _ =>
              "scala-2.13+"
          }
          (base / dir).getAbsoluteFile
        }
      },
      (console / initialCommands) += {
        "import scalaprops._;" + Seq(
          "Gen",
          "Cogen",
          "Rand"
        ).map(a => s"val $a = scalaprops.$a").mkString(";") // for tab completion
      }
    )
    .jvmPlatform(
      scalaVersions,
      Def.settings(
        jvm,
        jvmNative,
        Seq(Compile, Test).map { x =>
          x / unmanagedSourceDirectories += {
            (projectMatrixBaseDirectory.value / "jvm/src" / Defaults.nameForSrc(x.name) / "scala").getAbsoluteFile
          }
        },
        scalacOptions ++= {
          if (scalaVersion.value.startsWith("3.3.")) {
            Seq(
              "-Yfuture-lazy-vals",
              "-release:11"
            )
          } else {
            Nil
          }
        },
      )
    )
    .jsPlatform(
      scalaVersions,
      Def.settings(
        js,
        jsNative,
        Seq(Compile, Test).map { x =>
          x / unmanagedSourceDirectories += {
            (projectMatrixBaseDirectory.value / "js/src" / Defaults.nameForSrc(x.name) / "scala").getAbsoluteFile
          }
        },
        scalacOptions += {
          val a = (LocalRootProject / baseDirectory).value.toURI.toString
          val g = "https://raw.githubusercontent.com/scalaprops/scalaprops/" + tagOrHash.value
          val key = {
            if (isScala3.value) {
              "-scalajs-mapSourceURI"
            } else {
              "-P:scalajs:mapSourceURI"
            }
          }
          s"${key}:$a->$g/"
        }
      )
    )
    .nativePlatform(
      scalaVersions,
      Def.settings(
        jvmNative,
        jsNative,
        scalapropsNativeSettings,
        Seq(Compile, Test).map { x =>
          x / unmanagedSourceDirectories += {
            (projectMatrixBaseDirectory.value / "native/src" / Defaults.nameForSrc(x.name) / "scala").getAbsoluteFile
          }
        },
        native,
      )
    )

lazy val gen = module("gen").settings(
  name := genName,
  description := "pure functional random value generator",
  Generator.settings
)

lazy val core = module("core")
  .settings(
    name := coreName
  )
  .dependsOn(gen)

lazy val scalaz = module("scalaz")
  .settings(
    name := scalazName,
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.9",
  )
  .dependsOn(
    core,
    scalaprops % "test"
  )

lazy val scalaprops = module(
  id = scalapropsName,
  jvm = Def.settings(
    libraryDependencies += "org.scala-sbt" % "test-interface" % "1.0",
  ),
  js = Def.settings(
    libraryDependencies += ("org.scala-js" %% "scalajs-test-interface" % scalaJSVersion)
      .cross(CrossVersion.for3Use2_13)
      .platform(Platform.jvm)
  ),
  native = Def.settings(
    libraryDependencies += "org.scala-native" %% "test-interface" % nativeVersion,
  ),
).settings(
  name := scalapropsName
).dependsOn(
  core
)

val tagName = Def.setting {
  s"v${if (releaseUseGlobalVersion.value) (ThisBuild / version).value
    else version.value}"
}
val tagOrHash = Def.setting {
  if (isSnapshot.value) gitHash() else tagName.value
}

def gitHash(): String =
  sys.process.Process("git rev-parse HEAD").lazyLines_!.head

val unusedWarnings = Def.setting(
  scalaBinaryVersion.value match {
    case "2.12" =>
      Seq("-Ywarn-unused:imports,locals")
    case "2.13" =>
      Seq("-Wunused")
    case "3" =>
      Seq("-Wunused:all")
  }
)

def stripPom(filter: scala.xml.Node => Boolean): Setting[?] =
  pomPostProcess := { node =>
    import scala.xml._
    import scala.xml.transform._
    val rule = new RewriteRule {
      override def transform(n: Node) =
        if (filter(n)) NodeSeq.Empty else n
    }
    new RuleTransformer(rule).transform(node)(0)
  }

val commonSettings = Def.settings(
  _root_.scalaprops.ScalapropsPlugin.autoImport.scalapropsCoreSettings,
  (Compile / unmanagedResources) += (LocalRootProject / baseDirectory).value / "LICENSE.txt",
  publishTo := (if (isSnapshot.value) None else localStaging.value),
  organization := "com.github.scalaprops",
  description := "property based testing library for Scala",
  homepage := Some(url("https://github.com/scalaprops/scalaprops")),
  licenses := Seq("MIT License" -> url("https://opensource.org/licenses/mit-license")),
  commands += Command.command("updateReadme")(UpdateReadme.updateReadmeTask),
  stripPom { node => node.label == "dependency" && (node \ "scope").text == "test" },
  (Compile / doc / scalacOptions) ++= {
    val tag = tagOrHash.value
    if (isScala3.value) {
      Seq(
        "-source-links:github://scalaprops/scalaprops",
        "-revision",
        tag
      )
    } else {
      Seq(
        "-sourcepath",
        (LocalRootProject / baseDirectory).value.getAbsolutePath,
        "-doc-source-url",
        s"https://github.com/scalaprops/scalaprops/tree/${tag}€{FILE_PATH}.scala"
      )
    }
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
  javacOptions ++= Seq(
    "-source",
    "1.8",
    "-target",
    "1.8"
  ),
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-language:existentials,higherKinds,implicitConversions",
  ),
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) =>
        Seq(
          "-Wconf:msg=Implicit parameters should be provided with:error",
          "-Ykind-projector",
        )
      case Some((2, 13)) =>
        Seq(
          "-Xsource:3-cross",
        )
      case _ =>
        Seq(
          "-Xsource:3",
        )
    }
  },
  scalacOptions ++= PartialFunction
    .condOpt(CrossVersion.partialVersion(scalaVersion.value)) {
      case Some((2, v)) if v <= 12 =>
        Seq(
          "-Yno-adapted-args",
          "-Ypartial-unification",
          "-Xfuture"
        )
    }
    .toList
    .flatten,
  scalacOptions ++= unusedWarnings.value,
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
    releaseStepCommandAndRemaining(PgpKeys.publishSigned.key.label),
    releaseStepCommandAndRemaining("sonaRelease"),
    setNextVersion,
    commitNextVersion,
    UpdateReadme.updateReadmeProcess,
    pushChanges
  ),
) ++ Seq(Compile, Test).flatMap(c => (c / console / scalacOptions) --= unusedWarnings.value)

val all = Seq(gen, core, scalaz, scalaprops)

val scalapropsRoot = rootProject.autoAggregate
  .enablePlugins(
    ScalaUnidocPlugin
  )
  .settings(
    commonSettings,
    name := allName,
    artifacts := Nil,
    scalaVersion := Scala3,
    ScalaUnidoc / unidoc / unidocProjectFilter := {
      all.map(_.jvm(Scala3)).map(a => inProjects(a)).reduceLeft(_ && _)
    },
    packagedArtifacts := Def.uncached(Map.empty),
    artifacts ++= Classpaths.artifactDefs(Seq(Compile / packageDoc, Compile / makePom)).value,
    packagedArtifacts ++= Def.uncached(Classpaths.packaged(Seq(Compile / packageDoc, Compile / makePom)).value),
    description := "scalaprops unidoc",
    stripPom { _.label == "dependencies" },
    Seq[(String, ProjectMatrix => Seq[Project])](
      "JVM" -> (_.jvm.get),
      "JS" -> (_.js.get),
      "Native" -> (_.native.get),
    ).map { case (suffix, function) =>
      TaskKey[Unit]("testSequential" + suffix) := (
        Def
          .sequential(
            all.flatMap(function).map { x =>
              Def.sequential(
                Def.task(streams.value.log.info(s"start ${(x / thisProject).value.id} test")),
                x / Test / testFull,
                Def.task(streams.value.log.info(s"end ${(x / thisProject).value.id} test"))
              )
            }
          )
          .value
      )
    },
    Defaults.packageTaskSettings(
      (Compile / packageDoc),
      Def.task {
        given FileConverter = fileConverter.value
        (Compile / unidoc).value.flatMap(Mapper.allSubpaths)
      }
    ),
  )
