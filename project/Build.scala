object build {

  val genName = "scalaprops-gen"
  val coreName = "scalaprops-core"
  val allName = "scalaprops-all"
  val scalazlawsName = "scalaprops-scalazlaws"
  val scalapropsName = "scalaprops"

  val modules: List[String] = (
    genName ::
    coreName ::
    allName ::
    scalazlawsName ::
    scalapropsName ::
    Nil
  )
}
