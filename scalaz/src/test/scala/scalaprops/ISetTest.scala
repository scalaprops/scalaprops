package scalaprops

import Property.forAll
import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object ISetTest extends Scalaprops {
  property("foldable") = scalazlaws.foldable.all[ISet]

  property("order") = scalazlaws.order.all[ISet[Int]]

  property("test") = Properties.list(
    scalazlaws.monoid.all[ISet[Int]],
    scalazlaws.semilattice.all[ISet[Int]]
  )

  property("filter") = forAll { (a: ISet[Int], p: Int => Boolean) => (a filter p).toList == a.toList.filter(p) }

  property("partition") = forAll { (a: ISet[Int], p: Int => Boolean) =>
    val (x, y) = a partition p
    assert((x.size + y.size) == a.size)
    assert((x union y) == a)
    (x.toList, y.toList) == a.toList.partition(p)
  }
}
