addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.3.4")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.6.1")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "0.6.1")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.3.9")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.8.1")
addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.2")
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.12")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.0.1")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.31")
addSbtPlugin("com.github.scalaprops" % "sbt-scalaprops" % "0.3.2")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.3.0")

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Yno-adapted-args",
)

fullResolvers ~= { _.filterNot(_.name == "jcenter") }
