import UnidocKeys._
import build._

lazy val notPublish = Seq(
  publishArtifact := false,
  publish := {},
  publishLocal := {},
  PgpKeys.publishSigned := {},
  PgpKeys.publishLocalSigned := {}
)

lazy val jvmProjects = Seq[ProjectReference](
  genJVM, coreJVM, scalapropsJVM, scalazlawsJVM
)

lazy val jsProjects = Seq[ProjectReference](
  genJS, coreJS, scalapropsJS, scalazlawsJS
)

lazy val genJS = gen.js
lazy val genJVM = gen.jvm
lazy val genRoot = project.aggregate(genJS, genJVM).settings(
  notPublish
)

lazy val coreJS = core.js
lazy val coreJVM = core.jvm
lazy val coreRoot = project.aggregate(coreJS, coreJVM).settings(
  notPublish
)

lazy val scalazlawsJS = scalazlaws.js
lazy val scalazlawsJVM = scalazlaws.jvm
lazy val scalazlawsRoot = project.aggregate(scalazlawsJS, scalazlawsJVM).settings(
  notPublish
)

lazy val scalapropsJS = scalaprops.js
lazy val scalapropsJVM = scalaprops.jvm
lazy val scalapropsRoot = project.aggregate(scalapropsJS, scalapropsJVM).settings(
  notPublish
)

val root = Project("root", file(".")).settings(
  Common.commonSettings ++
  unidocSettings ++ (
    coreJVM ::
    scalapropsJVM ::
    scalazlawsJVM ::
    Nil
  ).map(p => libraryDependencies ++= (libraryDependencies in p).value)
).settings(
  name := allName,
  artifacts := Nil,
  unidocProjectFilter in (ScalaUnidoc, unidoc) := {
    jsProjects.foldLeft(inAnyProject)((acc, a) => acc -- inProjects(a))
  },
  packagedArtifacts := Map.empty,
  artifacts ++= Classpaths.artifactDefs(Seq(packageDoc in Compile, makePom in Compile)).value,
  packagedArtifacts ++= Classpaths.packaged(Seq(packageDoc in Compile, makePom in Compile)).value,
  description := "scalaprops unidoc",
  Common.stripPom { _.label == "dependencies" },
  Sxr.settings1,
  Defaults.packageTaskSettings(
    packageDoc in Compile, (UnidocKeys.unidoc in Compile).map{_.flatMap(Path.allSubpaths)}
  ),
  Sxr.settings2
).aggregate(
  jvmProjects ++ jsProjects : _*
)

lazy val rootJS = project.aggregate(jsProjects: _*).settings(
  notPublish
)
lazy val rootJVM = project.aggregate(jvmProjects: _*).settings(
  notPublish
)
