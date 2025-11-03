package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
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
