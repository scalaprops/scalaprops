package scalaprops

import scalaz.IList
import scalaz.std.anyVal._
import scalaz.std.string._

object IListTest extends Scalaprops {

  val testLaws = Properties.either(
    "IList",
    scalazlaws.monadPlus.all[IList],
    scalazlaws.traverse.all[IList],
    scalazlaws.cobind.all[IList],
    scalazlaws.isEmpty.all[IList],
    scalazlaws.align.all[IList],
    scalazlaws.zip.all[IList]
  )

}
