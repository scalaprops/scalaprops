package scalaprops

import scalaz._
import scalaz.std.anyVal._

object ISetTest extends Scalaprops {

  val testFoldable =
    scalazlaws.foldable.all[ISet]

  val testOrder =
    scalazlaws.order.all[ISet[Int]]

  val testMonoid =
    scalazlaws.monoid.all[ISet[Int]]

}
