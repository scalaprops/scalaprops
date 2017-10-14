import sbt._, Keys._
import Common._

import scalanative.sbtplugin.ScalaNativePlugin.autoImport._
import scalajscrossproject.ScalaJSCrossPlugin.autoImport.{toScalaJSGroupID => _, _}
import sbtcrossproject.CrossPlugin.autoImport._
import sbtcrossproject.CrossProject
import sbtcrossproject.CrossProject._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.scalaJSVersion

object build {

  private[this] val genName = "scalaprops-gen"
  private[this] val coreName = "scalaprops-core"
  val allName = "scalaprops-all"
  private[this] val scalazlawsName = "scalaprops-scalazlaws"
  private[this] val scalapropsName = "scalaprops"

  val scalazVersion = SettingKey[String]("scalazVersion")

  val modules: List[String] = (
    genName ::
    coreName ::
    allName ::
    scalazlawsName ::
    scalapropsName ::
    Nil
  )

  // avoid move files
  object CustomCrossType extends sbtcrossproject.CrossType {
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

  private[this] def module(id: String) =
    CrossProject(id, file(id), CustomCrossType, JSPlatform, JVMPlatform, NativePlatform).settings(
      commonSettings,
      scalazVersion := "7.2.16",
      initialCommands in console += {
        "import scalaprops._, scalaz._;" + Seq(
          "Gen", "Cogen", "Rand"
        ).map(a => s"val $a = scalaprops.$a").mkString(";") // for tab completion
      }
    ).jsSettings(
      scalacOptions += {
        val a = (baseDirectory in LocalRootProject).value.toURI.toString
        val g = "https://raw.githubusercontent.com/scalaprops/scalaprops/" + Common.tagOrHash.value
        s"-P:scalajs:mapSourceURI:$a->$g/"
      }
    ).nativeSettings(
      nativeGC := "immix"
    )

  lazy val gen = module("gen").settings(
    name := genName,
    description := "pure functional random value generator",
    libraryDependencies += "org.scalaz" %%% "scalaz-core" % scalazVersion.value
  ).platformsSettings(JSPlatform, NativePlatform)(
    unmanagedSourceDirectories in Compile += {
      baseDirectory.value.getParentFile / "js_native/src/main/scala/"
    }
  ).jvmSettings(
    Generator.settings
  )

  lazy val core = module("core").settings(
    name := coreName
  ).dependsOn(gen)

  lazy val scalazlaws = module("scalazlaws").settings(
    name := scalazlawsName
  ).dependsOn(core)

  lazy val scalaprops = module(scalapropsName).settings(
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
  ).dependsOn(
    core, scalazlaws % "test"
  ).jvmSettings(
    libraryDependencies += "org.scala-sbt" % "test-interface" % "1.0"
  ).platformsSettings(JVMPlatform, NativePlatform)(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided"
  ).jsSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-test-interface" % scalaJSVersion
  ).nativeSettings(
    libraryDependencies += "org.scala-native" %%% "test-interface" % nativeVersion
  )

}
