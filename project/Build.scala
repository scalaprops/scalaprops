import sbt._, Keys._
import sbtunidoc.Plugin._
import Common._
import org.scalajs.sbtplugin.cross.{CrossType, CrossProject}
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object build extends Build {

  private[this] val genName = "scalaprops-gen"
  private[this] val coreName = "scalaprops-core"
  private[this] val allName = "scalaprops-all"
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
      scalazVersion := "7.2.2",
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

  lazy val scalazSnapshotURI = uri("git://github.com/shawjef3/scalaz#ccd486287e903e269f83c92fcf7aabded0bb2760")

  lazy val gen = module("gen").settings(
    name := genName,
    description := "pure functional random value generator"
  ).jvmSettings(
    Generator.settings: _*
  )

  lazy val genJS = gen.js.dependsOn(ProjectRef(scalazSnapshotURI, "coreJS"))
  lazy val genJVM = gen.jvm.dependsOn(ProjectRef(scalazSnapshotURI, "coreJVM"))
  lazy val genRoot = project.aggregate(genJS, genJVM)

  lazy val core = module("core").settings(
    name := coreName
  ).dependsOn(gen)

  lazy val coreJS = core.js
  lazy val coreJVM = core.jvm
  lazy val coreRoot = project.aggregate(coreJS, coreJVM)

  lazy val scalazlaws = module("scalazlaws").settings(
    name := scalazlawsName
  ).dependsOn(core)

  lazy val scalazlawsJS = scalazlaws.js
  lazy val scalazlawsJVM = scalazlaws.jvm
  lazy val scalazlawsRoot = project.aggregate(scalazlawsJS, scalazlawsJVM)

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

  lazy val scalapropsJS = scalaprops.js
  lazy val scalapropsJVM = scalaprops.jvm
  lazy val scalapropsRoot = project.aggregate(scalapropsJS, scalapropsJVM)

  import UnidocKeys._

  private[this] lazy val jvmProjects = Seq[ProjectReference](
    genJVM, coreJVM, scalapropsJVM, scalazlawsJVM
  )
  private[this] lazy val jsProjects = Seq[ProjectReference](
    genJS, coreJS, scalapropsJS, scalazlawsJS
  )

  val root = Project("root", file(".")).settings(
    commonSettings ++
    unidocSettings ++
    xerial.sbt.Sonatype.sonatypeRootSettings ++ (
      coreJVM ::
      scalapropsJVM ::
      scalazlawsJVM ::
      Nil
    ).map(libraryDependencies <++= libraryDependencies in _)
  ).settings(
    name := allName,
    artifacts := Nil,
    unidocProjectFilter in (ScalaUnidoc, unidoc) := {
      jsProjects.foldLeft(inAnyProject)((acc, a) => acc -- inProjects(a))
    },
    packagedArtifacts := Map.empty,
    artifacts <++= Classpaths.artifactDefs(Seq(packageDoc in Compile)),
    packagedArtifacts <++= Classpaths.packaged(Seq(packageDoc in Compile)),
    Sxr.settings1,
    Defaults.packageTaskSettings(
      packageDoc in Compile, (UnidocKeys.unidoc in Compile).map{_.flatMap(Path.allSubpaths)}
    ),
    Sxr.settings2
  ).aggregate(
    jvmProjects ++ jsProjects : _*
  )

  lazy val rootJS = project.aggregate(jsProjects: _*)
  lazy val rootJVM = project.aggregate(jvmProjects: _*)
}
