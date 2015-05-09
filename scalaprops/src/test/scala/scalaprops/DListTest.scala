package scalaprops

import scalaz.DList
import scalaz.std.anyVal._
import scalaz.std.string._

object DListTest extends Scalaprops {

  val testLaws = Properties.either(
    "DList laws",
    scalazlaws.monadPlusStrong.all[DList],
    scalazlaws.zip.all[DList],
    scalazlaws.equal.all[DList[Int]],
    scalazlaws.traverse.all[DList]
  )

}
