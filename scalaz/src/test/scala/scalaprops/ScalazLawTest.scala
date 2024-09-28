package scalaprops

@scalajs.js.annotation.JSExportAll
object ScalazLawTest extends Scalaprops {
  val testLaws = scalazlaws.order.all[ScalazLaw]

  val testFullnameUnique = Property.forAll {
    ScalazLaw.values.map(_.fullName).distinct.size == ScalazLaw.values.size
  }
}
