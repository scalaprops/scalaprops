package scalaprops

import ScalapropsScalaz.*
import scalaz.ImmutableArray
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
object ImmutableArrayTest extends Scalaprops {
  val testEqual = scalazlaws.equal.all[ImmutableArray[Int]]
  val testFoldable = scalazlaws.foldable.all[ImmutableArray]
}
