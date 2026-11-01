package scalaprops

@scalajs.js.annotation.JSExportAll
object ScalazLawTest extends Scalaprops {
  property("laws") = scalazlaws.order.all[ScalazLaw]

  property("fullnameUnique") = Property.forAll {
    ScalazLaw.values.map(_.fullName).distinct.size == ScalazLaw.values.size
  }
}
