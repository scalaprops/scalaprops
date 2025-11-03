package scalaprops

import scalaz.std.anyVal.*
import scalaz.std.set.*

@scalajs.js.annotation.JSExportAll
object SetTest extends Scalaprops {
  val testLaws =
    Properties.list(
      scalazlaws.order.all[Set[Int]],
      scalazlaws.isEmpty.all[Set],
      scalazlaws.foldable.all[Set]
    )

  val test2 = scalazlaws.semilattice.all[Set[Int]]
}
