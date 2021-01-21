package scalaprops

object ShrinkTest extends Scalaprops {
  override val param: Param = super.param.copy(maxSize = 30)

  def law[A](a: A)(implicit s: Shrink[A]): Boolean =
    !s(a).contains(a)

  val boolean = Property.forAll(law[Boolean] _)
  val byte = Property.forAll(law[Byte] _)
  val short = Property.forAll(law[Short] _)
  val int = Property.forAll(law[Int] _)
  val long = Property.forAll(law[Long] _)
  val option = Property.forAll(law[Option[Byte]] _)
  val either = Property.forAll(law[Either[Byte, Byte]] _)
  val list = Property.forAll(law[List[Byte]] _)
  val tuple2 = Property.forAll(law[(Byte, Byte)] _)
  val tuple3 = Property.forAll(law[(Byte, Byte, Byte)] _)
  val tuple4 = Property.forAll(law[(Byte, Byte, Byte, Byte)] _)
  val map = Property.forAll(law[Map[Byte, Byte]] _)
  val array = Property.forAll(law[Array[Byte]] _)
  val bigInt = Property.forAll(law[BigInt] _)
  val bigInteger = Property.forAll(law[java.math.BigInteger] _)
}
