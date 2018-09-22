package scalaprops

import scalaz.ImmutableArray
import scalaz.std.anyVal._
import ScalapropsScalaz._

object ImmutableArrayTest extends Scalaprops {

  val testEqual = scalazlaws.equal.all[ImmutableArray[Int]]
  val testFoldable = scalazlaws.foldable.all[ImmutableArray]

}
