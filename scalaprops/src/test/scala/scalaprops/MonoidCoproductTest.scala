package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._

object MonoidCoproductTest extends Scalaprops {

  val testLaws =
    Properties.either(
      ":+:",
      scalazlaws.equal.all[Int :+: Int],
      scalazlaws.monoid.all[Int :+: Int]
    )

}
