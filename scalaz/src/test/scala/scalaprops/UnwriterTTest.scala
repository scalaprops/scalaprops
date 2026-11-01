package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*
import scalaz.std.tuple.*

object UnwriterTTest extends Scalaprops {
  property("id") = {
    type F[A] = Unwriter[Int, A]

    Properties.list(
      scalazlaws.bind.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.comonad.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  property("maybe1") = {
    type F[A] = UnwriterT[Maybe, Int, A]

    Properties.list(
      scalazlaws.bind.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  property("maybe2") = {
    type F[A, B] = UnwriterT[Maybe, A, B]

    scalazlaws.bitraverse.all[F]
  }
}
