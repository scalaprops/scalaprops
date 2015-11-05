import sbt._, Keys._
import sbtunidoc.Plugin._
import Common._

object build extends Build {

  private[this] val genName = "scalaprops-gen"
  private[this] val coreName = "scalaprops-core"
  private[this] val allName = "scalaprops-all"
  private[this] val scalazlawsName = "scalaprops-scalazlaws"
  private[this] val scalapropsName = "scalaprops"

  val scalazVersion = SettingKey[String]("scalazVersion")
  val shapelessVersion = SettingKey[String]("shapelessVersion")

  val modules: List[String] = (
    genName ::
    coreName ::
    allName ::
    scalazlawsName ::
    scalapropsName ::
    Nil
  )

  private[this] def module(id: String) =
    Project(id, file(id)).settings(commonSettings).settings(
      scalazVersion := "7.1.5",
      shapelessVersion := "2.2.5",
      initialCommands in console += "import scalaprops._, scalaz._"
    )

  lazy val gen = module("gen").settings(
    Generator.settings
  ).settings(
    name := genName,
    description := "pure functional random value generator",
    libraryDependencies += "org.scalaz" %% "scalaz-core" % scalazVersion.value
  )

  lazy val core = module("core").settings(
    name := coreName
  ).dependsOn(gen)

  lazy val scalazlaws = module("scalazlaws").settings(
    name := scalazlawsName
  ).dependsOn(core)

  lazy val scalaprops = module(scalapropsName).settings(
    name := scalapropsName,
    libraryDependencies += "org.scala-sbt" % "test-interface" % "1.0",
    libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % scalazVersion.value,
    shapelessDependency("test"),
    (sources in Test) := {
      val s = (sources in Test).value
      val useShapeless = Set("CofreeTest.scala", "FreeTest.scala")
      if(scalaVersion.value.startsWith("2.12")) {
        s.filterNot(f => useShapeless(f.getName))
      } else {
        s
      }
    },
    testFrameworks += new TestFramework("scalaprops.ScalapropsFramework"),
    parallelExecution in Test := false
  ).dependsOn(core, scalazlaws % "test")

  import UnidocKeys._

  val root = Project("root", file(".")).settings(
    commonSettings ++
    unidocSettings ++
    xerial.sbt.Sonatype.sonatypeRootSettings ++ (
      core ::
      scalaprops ::
      scalazlaws ::
      Nil
    ).map(libraryDependencies <++= libraryDependencies in _)
  ).settings(
    name := allName,
    artifacts := Nil,
    packagedArtifacts := Map.empty,
    artifacts <++= Classpaths.artifactDefs(Seq(packageDoc in Compile)),
    packagedArtifacts <++= Classpaths.packaged(Seq(packageDoc in Compile))
  ).settings(
    Sxr.settings1
  ).settings(
    Defaults.packageTaskSettings(
      packageDoc in Compile, (UnidocKeys.unidoc in Compile).map{_.flatMap(Path.allSubpaths)}
    )
  ).settings(
    Sxr.settings2
  ).aggregate(
    gen, core, scalaprops, scalazlaws
  )

}
