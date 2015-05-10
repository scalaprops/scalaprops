package scalaprops

import scalaz.\/
import scalaz.std.anyVal._
import scalaz.std.string._

object DisjunctionTest extends Scalaprops {

  val bitraverse = scalazlaws.bitraverse.all[\/]
  val associative = scalazlaws.associative.all[\/]
  val order = scalazlaws.order.all[Int \/ Int]

  val laws1 = Properties.either(
    """\/""",
    scalazlaws.monadError.all[\/, Int],
    scalazlaws.traverse.all[({type l[a] = Int \/ a})#l],
    scalazlaws.plus.all[({type l[a] = Int \/ a})#l]
  )
}
