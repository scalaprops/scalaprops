package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._

object ValidationTest extends Scalaprops {

  val testLaws1 = {
    type F[A] = ValidationNel[Int, A]

    Properties.either(
      "Validation",
      scalazlaws.applicative.all[F],
      scalazlaws.plus.all[F]
    )
  }

  val testLaws2 = scalazlaws.associative.all[Validation]
  val testLaws3 = scalazlaws.bitraverse.all[Validation]
  val testLaws4 = scalazlaws.monoid.all[Validation[Int, Int]]
  val testOrder = scalazlaws.order.all[Validation[Int, Int]]
}
