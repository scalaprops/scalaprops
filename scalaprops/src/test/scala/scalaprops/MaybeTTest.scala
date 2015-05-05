package scalaprops

import scalaz._
import scalaz.std.anyVal._

object MaybeTTest extends Scalaprops {

  val testMonadLaws =
    scalazlaws.monadPlus.all[({type l[a] = MaybeT[IList, a]})#l]
}
