addSbtPlugin("org.scala-native" % "sbt-scalajs-crossproject" % "0.2.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.3.0")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "1.1")
addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.3.2")
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.5")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.1")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.16")
addSbtPlugin("com.github.scalaprops" % "sbt-scalaprops" % "0.2.2")

scalacOptions ++= (
  "-deprecation" ::
  "-unchecked" ::
  "-Xlint" ::
  "-language:existentials" ::
  "-language:higherKinds" ::
  "-language:implicitConversions" ::
  "-Yno-adapted-args" ::
  Nil
)

fullResolvers ~= {_.filterNot(_.name == "jcenter")}
