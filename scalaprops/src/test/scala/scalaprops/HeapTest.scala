package scalaprops

import scalaz.Heap
import scalaz.std.anyVal._

object HeapTest extends Scalaprops {

  val testMonoid = scalazlaws.monoid.all[Heap[Int]]

  val testEqual = scalazlaws.equal.all[Heap[Int]]

  val testFoldable = scalazlaws.foldable.all[Heap]

}
