package scalaprops

import ScalapropsScalaz.*
import scalaz.std.anyVal.*
import scalaz.std.map.*

object MapTest extends Scalaprops {
  property("laws1") = {
    type F[A] = Map[Int, A]
    Properties.list(
      scalazlaws.bind.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.isEmpty.all[F],
      scalazlaws.align.all[F],
      scalazlaws.order.all[Map[Int, Int]]
    )
  }

  property("laws2") = scalazlaws.monoid.all[Map[Int, Int]]
}
