import sbt._, Keys._
import Common._

object build {

  val genName = "scalaprops-gen"
  val coreName = "scalaprops-core"
  val allName = "scalaprops-all"
  val scalazlawsName = "scalaprops-scalazlaws"
  val scalapropsName = "scalaprops"

  val scalazVersion = SettingKey[String]("scalazVersion")

  val modules: List[String] = (
    genName ::
    coreName ::
    allName ::
    scalazlawsName ::
    scalapropsName ::
    Nil
  )

  def module(id: String) =
    Project(id, file(id)).settings(commonSettings).settings(
      scalazVersion := "7.1.13",
      initialCommands in console += {
        "import scalaprops._, scalaz._;" + Seq(
          "Gen", "Cogen", "Rand"
        ).map(a => s"val $a = scalaprops.$a").mkString(";") // for tab completion
      }
    )
}
