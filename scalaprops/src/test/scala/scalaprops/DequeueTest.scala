package scalaprops

import scalaz._
import scalaz.std.string._
import scalaz.std.anyVal._

object DequeueTest extends Scalaprops {

  val testLaws =
    Properties.either(
      "Dequeue",
      scalazlaws.functor.all[Dequeue],
      scalazlaws.foldable.all[Dequeue],
      scalazlaws.isEmpty.all[Dequeue]
    )

}
