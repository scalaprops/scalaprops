package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object LazyOptionTTest extends Scalaprops {
  property("iList") = Properties.list(
    scalazlaws.equal.all[LazyOptionT[IList, Int]],
    scalazlaws.monadPlus.all[({ type l[a] = LazyOptionT[IList, a] })#l]
  )

  property("bindRecIList") =
    scalazlaws.bindRec.all[({ type l[a] = LazyOptionT[IList, a] })#l].andThenParam(Param.maxSize(1))

  property("maybe") = Properties.list(
    scalazlaws.equal.all[LazyOptionT[Maybe, Int]],
    scalazlaws.bindRec.all[({ type l[a] = LazyOptionT[Maybe, a] })#l],
    scalazlaws.monadPlus.all[({ type l[a] = LazyOptionT[Maybe, a] })#l]
  )

  property("monadTrans") = scalazlaws.monadTrans.all[LazyOptionT]
}
