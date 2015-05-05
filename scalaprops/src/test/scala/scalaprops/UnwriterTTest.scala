package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._
import scalaz.std.tuple._

object UnwriterTTest extends Scalaprops {

  val testMaybe1 = {
    type F[A] = UnwriterT[Maybe, Int, A]

    Properties.either(
      "UnwriterT[Maybe, Int, _]",
      scalazlaws.bind.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  val testMaybe2 = {
    type F[A, B] = UnwriterT[Maybe, A, B]

    Properties.either(
      "UnwriterT[Maybe, _, _]",
      scalazlaws.bitraverse.all[F]
    )
  }

}
