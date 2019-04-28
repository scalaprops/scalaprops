package scalaprops

import scalaz._
import scalaz.std.anyVal._
import ScalapropsScalaz._

object ValidationTest extends Scalaprops {

  val testLaws1 = {
    type F[A] = ValidationNel[Int, A]

    Properties.list(
      scalazlaws.applicative.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.plus.all[F]
    )
  }

  val testLaws2 = scalazlaws.associative.all[Validation]
  val testLaws3 = scalazlaws.bitraverse.all[Validation]
  val testLaws4 = scalazlaws.monoid.all[Validation[Int, Int]]
  val testOrder = scalazlaws.order.all[Validation[Int, Int]]
}
