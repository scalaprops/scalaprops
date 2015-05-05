package scalaprops

import scalaz.std.map._
import scalaz.std.anyVal._
import scalaz.std.string._

object MapTest extends Scalaprops {

  val testLaws1 = {
    type F[A] = Map[Int, A]
    Properties.either(
      "Map[Int, _]",
      scalazlaws.bind.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.isEmpty.all[F],
      scalazlaws.align.all[F],
      scalazlaws.order.all[Map[Int, Int]]
    )
  }

  val testLaws2 = scalazlaws.monoid.all[Map[Int, Int]]
}
