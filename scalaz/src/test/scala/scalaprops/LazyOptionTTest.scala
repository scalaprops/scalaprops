package scalaprops

import scalaz._
import scalaz.std.anyVal._
import ScalapropsScalaz._

object LazyOptionTTest extends Scalaprops {
  val iList = Properties.list(
    scalazlaws.equal.all[LazyOptionT[IList, Int]],
    scalazlaws.monadPlus.all[({ type l[a] = LazyOptionT[IList, a] })#l]
  )

  val bindRecIList =
    scalazlaws.bindRec.all[({ type l[a] = LazyOptionT[IList, a] })#l].andThenParam(Param.maxSize(1))

  val maybe = Properties.list(
    scalazlaws.equal.all[LazyOptionT[Maybe, Int]],
    scalazlaws.bindRec.all[({ type l[a] = LazyOptionT[Maybe, a] })#l],
    scalazlaws.monadPlus.all[({ type l[a] = LazyOptionT[Maybe, a] })#l]
  )

  val monadTrans = scalazlaws.monadTrans.all[LazyOptionT]
}
