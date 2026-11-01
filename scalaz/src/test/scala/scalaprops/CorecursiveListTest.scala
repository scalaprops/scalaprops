package scalaprops

import ScalapropsScalaz.*
import scalaz.CorecursiveList
import scalaz.std.anyVal.*

object CorecursiveListTest extends Scalaprops {
  property("laws") = Properties.list(
    scalazlaws.order.all[CorecursiveList[Byte]],
    scalazlaws.monadPlusStrong.all[CorecursiveList],
    scalazlaws.foldable.all[CorecursiveList],
    scalazlaws.isEmpty.all[CorecursiveList],
    scalazlaws.align.all[CorecursiveList],
    scalazlaws.zip.all[CorecursiveList]
  )
}
