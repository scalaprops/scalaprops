package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._

object IndSeqTest extends Scalaprops {

  val testLaw =
    Properties.either(
      "IndSeq",
      scalazlaws.monadPlus.all[IndSeq],
      scalazlaws.traverse.all[IndSeq],
      scalazlaws.isEmpty.all[IndSeq]
    )

}
