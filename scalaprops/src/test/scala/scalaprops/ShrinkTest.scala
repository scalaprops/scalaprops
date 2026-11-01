package scalaprops

object ShrinkTest extends Scalaprops {
  override val param: Param = super.param.copy(maxSize = 30)

  def law[A](a: A)(implicit s: Shrink[A]): Boolean =
    !s(a).contains(a)

  property("boolean") = Property.forAll(law[Boolean] _)
  property("byte") = Property.forAll(law[Byte] _)
  property("short") = Property.forAll(law[Short] _)
  property("int") = Property.forAll(law[Int] _)
  property("long") = Property.forAll(law[Long] _)
  property("option") = Property.forAll(law[Option[Byte]] _)
  property("either") = Property.forAll(law[Either[Byte, Byte]] _)
  property("list") = Property.forAll(law[List[Byte]] _)
  property("tuple2") = Property.forAll(law[(Byte, Byte)] _)
  property("tuple3") = Property.forAll(law[(Byte, Byte, Byte)] _)
  property("tuple4") = Property.forAll(law[(Byte, Byte, Byte, Byte)] _)
  property("map") = Property.forAll(law[Map[Byte, Byte]] _)
  property("array") = Property.forAll(law[Array[Byte]] _)
  property("bigInt") = Property.forAll(law[BigInt] _)
  property("bigInteger") = Property.forAll(law[java.math.BigInteger] _)
}
