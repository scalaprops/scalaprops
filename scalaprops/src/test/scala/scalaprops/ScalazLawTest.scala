package scalaprops

object ScalazLawTest extends Scalaprops {
  val testLaws = scalazlaws.order.all[ScalazLaw]

  val testFullnameUnique = Property.prop{
    ScalazLaw.values.map(_.fullName).distinct.size == ScalazLaw.values.size
  }
}
