package scalaprops

import scalaz.\/
import scalaz.std.anyVal._

object DisjunctionTest extends Scalaprops {

  val bitraverse = scalazlaws.bitraverse.all[\/]
  val associative = scalazlaws.associative.all[\/]
  val order = scalazlaws.order.all[Int \/ Int]

  val laws1 = Properties.list(
    scalazlaws.monadError.all[({type l[a] = Int \/ a})#l, Int],
    scalazlaws.traverse.all[({type l[a] = Int \/ a})#l],
    scalazlaws.bindRec.all[({type l[a] = Int \/ a})#l],
    scalazlaws.plus.all[({type l[a] = Int \/ a})#l]
  )
}
