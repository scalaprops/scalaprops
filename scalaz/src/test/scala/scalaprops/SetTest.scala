package scalaprops

import scalaz.std.anyVal.*
import scalaz.std.set.*

object SetTest extends Scalaprops {
  property("laws") = Properties.list(
    scalazlaws.order.all[Set[Int]],
    scalazlaws.isEmpty.all[Set],
    scalazlaws.foldable.all[Set]
  )

  property("test2") = scalazlaws.semilattice.all[Set[Int]]
}
