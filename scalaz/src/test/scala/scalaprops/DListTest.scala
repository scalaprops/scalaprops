package scalaprops

import scalaz.DList
import scalaz.std.anyVal._
import ScalapropsScalaz._

@scalajs.js.annotation.JSExportAll
object DListTest extends Scalaprops {
  val testLaws = Properties.list(
    scalazlaws.monadPlusStrong.all[DList],
    scalazlaws.zip.all[DList],
    scalazlaws.isEmpty.all[DList],
    scalazlaws.equal.all[DList[Int]],
    scalazlaws.traverse.all[DList]
  )

  val bindRec = scalazlaws.bindRec.laws[DList].andThenParam(Param.maxSize(1))
}
