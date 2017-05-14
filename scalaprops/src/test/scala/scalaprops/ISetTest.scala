package scalaprops

import scalaz._
import scalaz.std.anyVal._
import Property.forAll

object ISetTest extends Scalaprops {

  val testFoldable =
    scalazlaws.foldable.all[ISet]

  val testOrder =
    scalazlaws.order.all[ISet[Int]]

  val test = Properties.list(
    scalazlaws.monoid.all[ISet[Int]],
    scalazlaws.band.all[ISet[Int]]
  )

  val filter = forAll { (a: ISet[Int], p: Int => Boolean) =>
    (a filter p).toList == a.toList.filter(p)
  }

  val partition = forAll { (a: ISet[Int], p: Int => Boolean) =>
    val (x, y) = a partition p
    assert((x.size + y.size) == a.size)
    assert((x union y) == a)
    (x.toList, y.toList) == a.toList.partition(p)
  }

}
