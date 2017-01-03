import sbtunidoc.Plugin._
import build._
import Common._
import UnidocKeys._

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
  libraryDependencies += "org.scala-sbt" % "test-interface" % "1.0"
).dependsOn(core, scalazlaws % "test")


val root = Project("root", file(".")).settings(
  commonSettings ++
  unidocSettings ++ (
    core ::
    scalaprops ::
    scalazlaws ::
    Nil
  ).map(p => libraryDependencies ++= (libraryDependencies in p).value)
).settings(
  name := allName,
  artifacts := Nil,
  packagedArtifacts := Map.empty,
  artifacts ++= Classpaths.artifactDefs(Seq(packageDoc in Compile)).value,
  packagedArtifacts ++= Classpaths.packaged(Seq(packageDoc in Compile)).value
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
