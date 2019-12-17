package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.lazylist._
import ScalapropsScalaz._

object LazyListTest extends Scalaprops {

  val bindRec = scalazlaws.bindRec.laws[LazyList].andThenParam(Param.maxSize(2))

  val laws = Properties.list(
    scalazlaws.monadPlusStrong.all[LazyList],
    scalazlaws.align.all[LazyList],
    scalazlaws.zip.all[LazyList],
    scalazlaws.isEmpty.all[LazyList],
    scalazlaws.cobind.all[LazyList],
    scalazlaws.traverse.all[LazyList]
  )

}
