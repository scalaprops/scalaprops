import sbt._, Keys._
import sbtunidoc.Plugin._
import Common._
import org.scalajs.sbtplugin.cross.{CrossType, CrossProject}
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

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
  // https://github.com/scala-js/scala-js/blob/v0.6.8/sbt-plugin/src/main/scala/scala/scalajs/sbtplugin/cross/CrossProject.scala#L193-L206
  object CustomCrossType extends CrossType {
    override def projectDir(crossBase: File, projectType: String) =
      crossBase / projectType

    def shared(projectBase: File, conf: String) =
      projectBase.getParentFile / "src" / conf / "scala"

    override def sharedSrcDir(projectBase: File, conf: String) =
      Some(shared(projectBase, conf))
  }

  private[this] def module(id: String) =
    CrossProject(id, file(id), CustomCrossType).settings(
      commonSettings: _*
    ).settings(
      scalazVersion := "7.2.4",
      scalaJSStage in Test := FastOptStage,
      scalaJSUseRhino in Global := false,
      jsEnv := NodeJSEnv().value,
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
    )

  lazy val gen = module("gen").settings(
    name := genName,
    description := "pure functional random value generator",
    libraryDependencies += "org.scalaz" %%% "scalaz-core" % scalazVersion.value
  ).jvmSettings(
    Generator.settings: _*
  )

  lazy val core = module("core").settings(
    name := coreName
  ).dependsOn(gen)

  lazy val scalazlaws = module("scalazlaws").settings(
    name := scalazlawsName
  ).dependsOn(core)

  lazy val neko = module("neko").settings(
    name := "neko",
    libraryDependencies += "org.typelevel" %%% "cats-laws" % "0.6.1"
  ).dependsOn(
    core,
    scalaprops % "test"
  )

  lazy val scalaprops = module(scalapropsName).settings(
    name := scalapropsName
  ).dependsOn(
    core, scalazlaws % "test"
  ).jvmSettings(
    libraryDependencies += "org.scala-sbt" % "test-interface" % "1.0",
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided"
  ).jsSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-test-interface" % scalaJSVersion
  )

}
