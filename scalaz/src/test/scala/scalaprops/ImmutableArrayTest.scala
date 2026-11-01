package scalaprops

import ScalapropsScalaz.*
import scalaz.ImmutableArray
import scalaz.std.anyVal.*

object ImmutableArrayTest extends Scalaprops {
  property("equal") = scalazlaws.equal.all[ImmutableArray[Int]]
  property("foldable") = scalazlaws.foldable.all[ImmutableArray]
}
