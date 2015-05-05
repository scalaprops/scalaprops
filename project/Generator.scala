import sbt._, Keys._

object Generator {

  private[this] val generateCode = TaskKey[Unit]("generateCode")
  private[this] val generateFiles = SettingKey[Seq[GeneratedCode]]("generateFiles")
  private[this] val checkGenerateCode = TaskKey[Boolean]("checkGenerateCode")
  private[this] val checkGenerateCodeError = TaskKey[Unit]("checkGenerateCodeError")

  private[this] final case class GeneratedCode(file: File, code: String) {
    def write(): Unit = IO.write(file, code)
    def check: Boolean = {
      if(file.isFile){
        IO.read(file) == code
      }else{
        println(red(file + " not found!"))
        false
      }
    }
  }

  private[this] def red(str: String) = {
    ("\n" * 2) + scala.Console.RED + str + ("\n " * 2) + scala.Console.RESET
  }

  val settings: Seq[Def.Setting[_]] = Seq(
    generateFiles := {
      val pack = "scalaprops"
      val dir = (scalaSource in Compile).value / pack
      def code(name: String, code: String) = GeneratedCode(dir / name, code)
      List(
        code("CogenInstances.scala", Cogen.gen),
        code("GenInstances.scala", Gen.gen)
      )
    },
    generateCode := generateFiles.value.foreach(_.write()),
    checkGenerateCode := generateFiles.value.forall(_.check),
    checkGenerateCodeError := {
      generateCode.value
      val diff = "git diff".!!
      if(diff.nonEmpty){
        sys.error("Working directory is dirty!\n" + diff)
      }
    },
    shellPrompt := { state =>
      val extracted = Project.extract(state)
      if(extracted.runTask(checkGenerateCode, state)._2){
        shellPrompt.?.value.map(_.apply(state)).getOrElse("")
      }else{
        red("generator code changed. please execute " + generateCode.key.label)
      }
    }
  )
}
