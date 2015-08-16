package scalaprops

import scalaz._
import scalaz.std.AllInstances._

object IsoTest extends Scalaprops {

  /**
   * @see [[http://docs.oracle.com/javase/8/docs/api/java/math/BigInteger.html#BigInteger-byte:A-]]
   * @see [[http://docs.oracle.com/javase/8/docs/api/java/math/BigInteger.html#toByteArray--]]
   */
  private[this] val genByteNel =
    Gen[NonEmptyList[Byte]].map { bytes =>
      val n: Byte = if (bytes.head >= 0) 0 else -1
      bytes.list.dropWhile(_ == n) match {
        case ICons(h, t) =>
          NonEmptyList.nel(h, t)
        case INil() =>
          NonEmptyList.nel[Byte](n, IList.empty)
      }
    }

  val bigInt = scalazlaws.iso.all(Iso.bigInt)(
    genByteNel, implicitly,implicitly,implicitly
  )

  val maybe = scalazlaws.iso.all(Iso.maybe.unlift[Byte])
  val iList = scalazlaws.iso.all(Iso.iList.unlift[Byte])
  val boolean = scalazlaws.iso.all(Iso.boolean)
  val int = scalazlaws.iso.all(Iso.int)
  val nel = scalazlaws.iso.all(Iso.nel.unlift[Byte])
  val either = scalazlaws.iso.all(Iso.either.unlift[Byte, Byte])
  val tuple4 = scalazlaws.iso.all(Iso.tuple4[Byte, Byte, Byte, Byte])

}
