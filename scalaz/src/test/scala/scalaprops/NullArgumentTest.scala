package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object NullArgumentTest extends Scalaprops {
  import FunctionEqual.*

  private[this] implicit def equal[A, B](implicit A: Equal[Option[A] => B]): Equal[NullArgument[A, B]] =
    A.contramap(_.apply)

  property("laws") = {
    type F[A] = NullArgument[Byte, A]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.bindRec.all[F]
    )
  }

  property("compose") = scalazlaws.compose.all[NullArgument]
  property("contravariant") = scalazlaws.contravariant.all[({ type l[a] = NullArgument[a, Int] })#l]
  property("monoid") = scalazlaws.monoid.all[NullArgument[Int, Int]]
}
