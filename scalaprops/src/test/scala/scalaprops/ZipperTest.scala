package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._

object ZipperTest extends Scalaprops {

  val testLaw =
    Properties.either(
      "Zipper",
      scalazlaws.comonad.all[Zipper],
      scalazlaws.apply.all[Zipper],
      scalazlaws.traverse.all[Zipper],
      scalazlaws.equal.all[Zipper[Int]]
    )

}
