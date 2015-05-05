package scalaprops

import scalaz.\/
import scalaz.std.anyVal._
import scalaz.std.string._

object DisjunctionTest extends Scalaprops {

  val testBifunctor = scalazlaws.bifunctor.all[\/]
  val testBifoldable = scalazlaws.bifoldable.all[\/]

  val testLaws1 = Properties.either(
    """\/""",
    scalazlaws.monadError.all[\/, Int],
    scalazlaws.traverse.all[({type l[a] = Int \/ a})#l],
    scalazlaws.plus.all[({type l[a] = Int \/ a})#l]
  )
}
