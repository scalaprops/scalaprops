package scalaprops

import scalaz._
import scalaz.std.anyVal._

object ZipperTest extends Scalaprops {

  val testLaw =
    Properties.list(
      scalazlaws.comonad.all[Zipper],
      scalazlaws.apply.all[Zipper],
      scalazlaws.traverse.all[Zipper],
      scalazlaws.equal.all[Zipper[Int]]
    )

}
