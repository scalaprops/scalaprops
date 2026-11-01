package scalaprops

import ScalapropsScalaz.*
import scalaz.Heap
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
object HeapTest extends Scalaprops {
  property("monoid") = scalazlaws.monoid.all[Heap[Int]]

  property("equal") = scalazlaws.equal.all[Heap[Int]]

  property("foldable") = scalazlaws.foldable.all[Heap]
}
