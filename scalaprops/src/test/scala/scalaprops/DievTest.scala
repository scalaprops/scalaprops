package scalaprops

import scalaz.Diev
import scalaz.std.anyVal._

object DievTest extends Scalaprops {

  val testLaw =
    Properties.list(
      scalazlaws.equal.all[Diev[Int]],
      scalazlaws.monoid.all[Diev[Int]]
    )

}
