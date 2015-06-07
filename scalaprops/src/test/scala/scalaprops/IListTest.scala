package scalaprops

import scalaz.IList
import scalaz.std.anyVal._

object IListTest extends Scalaprops {

  val testLaws = Properties.list(
    scalazlaws.monadPlusStrong.all[IList],
    scalazlaws.traverse.all[IList],
    scalazlaws.cobind.all[IList],
    scalazlaws.isEmpty.all[IList],
    scalazlaws.align.all[IList],
    scalazlaws.zip.all[IList]
  )

}
