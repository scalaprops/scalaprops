package scalaprops

import scalaz.CorecursiveList
import scalaz.std.anyVal._
import ScalapropsScalaz._

object CorecursiveListTest extends Scalaprops {
  val testLaws = Properties.list(
    scalazlaws.order.all[CorecursiveList[Byte]],
    scalazlaws.monadPlusStrong.all[CorecursiveList],
    scalazlaws.foldable.all[CorecursiveList],
    scalazlaws.isEmpty.all[CorecursiveList],
    scalazlaws.align.all[CorecursiveList],
    scalazlaws.zip.all[CorecursiveList]
  )
}
