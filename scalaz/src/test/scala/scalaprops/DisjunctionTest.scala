package scalaprops

import scalaz.{\/, IList}
import scalaz.std.anyVal._
import ScalapropsScalaz._

object DisjunctionTest extends Scalaprops {

  val bitraverse = scalazlaws.bitraverse.all[\/]
  val associative = scalazlaws.associative.all[\/]
  val order = scalazlaws.order.all[Int \/ Int]
  val monoid = scalazlaws.monoid.all[IList[Byte] \/ IList[Byte]]

  val laws1 = Properties.list(
    scalazlaws.monadError.all[({type l[a] = Int \/ a})#l, Int],
    scalazlaws.traverse.all[({type l[a] = Int \/ a})#l],
    scalazlaws.bindRec.all[({type l[a] = Int \/ a})#l],
    scalazlaws.plus.all[({type l[a] = Int \/ a})#l]
  )
}
