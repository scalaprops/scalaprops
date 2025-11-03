package scalaprops

import ScalapropsScalaz.*
import scalaz.CorecursiveList
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
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
