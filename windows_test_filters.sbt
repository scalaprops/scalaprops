val excludeTestsIfWindows = Set(
  "scalaprops.PropertiesTest"
)

testOptions in Test in ThisBuild ++= {
  if (scala.util.Properties.isWin) {
    Seq(Tests.Exclude(excludeTestsIfWindows))
  } else {
    Nil
  }
}
