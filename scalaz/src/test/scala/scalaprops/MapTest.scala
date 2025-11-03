package scalaprops

import ScalapropsScalaz.*
import scalaz.std.anyVal.*
import scalaz.std.map.*

@scalajs.js.annotation.JSExportAll
object MapTest extends Scalaprops {
  val testLaws1 = {
    type F[A] = Map[Int, A]
    Properties.list(
      scalazlaws.bind.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.isEmpty.all[F],
      scalazlaws.align.all[F],
      scalazlaws.order.all[Map[Int, Int]]
    )
  }

  val testLaws2 = scalazlaws.monoid.all[Map[Int, Int]]
}
