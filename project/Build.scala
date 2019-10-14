object build {

  val genName = "scalaprops-gen"
  val coreName = "scalaprops-core"
  val allName = "scalaprops-all"
  val scalazName = "scalaprops-scalaz"
  val scalapropsName = "scalaprops"

  val modules: List[String] = (
    genName ::
      coreName ::
      allName ::
      scalazName ::
      scalapropsName ::
      Nil
  )
}
