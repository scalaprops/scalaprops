package scalaprops

import scalaz._
import scalaz.std.anyVal._
import ScalapropsScalaz._

object MonoidCoproductTest extends Scalaprops {
  val testLaws =
    Properties.list(
      scalazlaws.equal.all[Int :+: Int],
      scalazlaws.monoid.all[Int :+: Int]
    )
}
