package scalaprops

import ScalapropsScalaz.*
import scalaz.DList
import scalaz.std.anyVal.*

object DListTest extends Scalaprops {
  property("laws") = Properties.list(
    scalazlaws.monadPlusStrong.all[DList],
    scalazlaws.zip.all[DList],
    scalazlaws.isEmpty.all[DList],
    scalazlaws.equal.all[DList[Int]],
    scalazlaws.traverse.all[DList]
  )

  property("bindRec") = scalazlaws.bindRec.laws[DList].andThenParam(Param.maxSize(1))
}
