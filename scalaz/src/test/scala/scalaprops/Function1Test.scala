package scalaprops

import scalaz.std.anyVal.*
import scalaz.std.function.*
import scalaz.std.tuple.*

object Function1Test extends Scalaprops {
  import FunctionEqual.*

  property("bindRec") = scalazlaws.bindRec.laws[({ type l[a] = Byte => a })#l]

  property("laws1") = {
    type F1[A] = Int => A
    type F2[A] = A => Int

    Properties.list(
      scalazlaws.monad.all[F1],
      scalazlaws.comonad.all[F1],
      scalazlaws.zip.all[F1],
      scalazlaws.contravariant.all[F2],
      scalazlaws.arrow.all[Function1]
    )
  }

  property("laws2") = scalazlaws.monoid.all[Int => Int]
}
