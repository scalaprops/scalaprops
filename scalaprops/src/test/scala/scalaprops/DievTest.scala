package scalaprops

import scalaz.Diev
import scalaz.std.anyVal._
import scalaz.std.string._

object DievTest extends Scalaprops {

  val testLaw =
    Properties.either(
      "Diev[Int]",
      scalazlaws.equal.all[Diev[Int]],
      scalazlaws.monoid.all[Diev[Int]]
    )

}
