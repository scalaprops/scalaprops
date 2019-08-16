package scalaprops

object Issue48 extends Scalaprops {
  val issue48test = Property.exception {
    Property.prop {
      ScalapropsRunner.testFieldNames(Issue48.getClass).toList == List("issue48test")
    }
  }
}
