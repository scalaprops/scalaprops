package scalaprops

import scalaz.std.set._
import scalaz.std.anyVal._
import scalaz.std.string._

object SetTest extends Scalaprops {

  val testLaws =
    Properties.either(
      "Set",
      scalazlaws.order.all[Set[Int]],
      scalazlaws.isEmpty.all[Set],
      scalazlaws.foldable.all[Set]
    )

}
