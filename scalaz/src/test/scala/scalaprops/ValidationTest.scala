package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object ValidationTest extends Scalaprops {
  property("laws1") = {
    type F[A] = ValidationNel[Int, A]

    Properties.list(
      scalazlaws.applicative.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.plus.all[F]
    )
  }

  property("laws2") = scalazlaws.associative.all[Validation]
  property("laws3") = scalazlaws.bitraverse.all[Validation]
  property("laws4") = scalazlaws.monoid.all[Validation[Int, Int]]
  property("order") = scalazlaws.order.all[Validation[Int, Int]]
}
