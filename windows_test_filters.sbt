val excludeTestsIfWindows = Set(
  "scalaprops.PropertiesTest"
)

ThisBuild / Test / testOptions ++= {
  if (scala.util.Properties.isWin) {
    Seq(Tests.Exclude(excludeTestsIfWindows))
  } else {
    Nil
  }
}
