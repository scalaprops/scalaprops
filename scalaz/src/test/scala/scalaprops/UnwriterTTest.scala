package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.tuple._
import ScalapropsScalaz._

object UnwriterTTest extends Scalaprops {
  val id = {
    type F[A] = Unwriter[Int, A]

    Properties.list(
      scalazlaws.bind.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.comonad.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  val testMaybe1 = {
    type F[A] = UnwriterT[Maybe, Int, A]

    Properties.list(
      scalazlaws.bind.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  val testMaybe2 = {
    type F[A, B] = UnwriterT[Maybe, A, B]

    scalazlaws.bitraverse.all[F]
  }
}
