def addScalafmtPluginTask(taskName: String, tasks: List[String]): Seq[Def.Setting[_]] = {
  val moduleId = """ "com.lucidchart" % "sbt-scalafmt" % "1.15" """
  val removeCommand = "removeTemporary" + taskName

  def tempPluginSbtFile(base: File) =
    base / "project" / ("temporaryScalafmt.sbt")

  Def.settings(
    TaskKey[Unit](removeCommand) := {
      val f = tempPluginSbtFile((baseDirectory in LocalRootProject).value)
      IO.delete(f)
    },
    commands += Command.command(taskName) { state =>
      val extracted = Project.extract(state)
      val f = tempPluginSbtFile(extracted.get(baseDirectory in LocalRootProject))
      IO.write(f, "addSbtPlugin(" + moduleId + ")")
      "reload" :: tasks ::: removeCommand :: "reload" :: state
    }
  )
}

addScalafmtPluginTask(
  "scalafmtCheckAll",
  List("scalafmt::test", "sbt:scalafmt::test", "test:scalafmt::test")
)

addScalafmtPluginTask(
  "scalafmtAll",
  List("scalafmt", "sbt:scalafmt", "test:scalafmt")
)
