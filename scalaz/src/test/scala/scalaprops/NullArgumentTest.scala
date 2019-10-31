package scalaprops

import scalaz._
import scalaz.std.anyVal._
import ScalapropsScalaz._

object NullArgumentTest extends Scalaprops {
  import FunctionEqual._

  private[this] implicit def equal[A, B](implicit A: Equal[Option[A] => B]): Equal[NullArgument[A, B]] =
    A.contramap(_.apply)

  val laws = {
    type F[A] = NullArgument[Byte, A]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.bindRec.all[F]
    )
  }

  val testCompose = scalazlaws.compose.all[NullArgument]
  val testContravariant = scalazlaws.contravariant.all[({ type l[a] = NullArgument[a, Int] })#l]
  val testMonoid = scalazlaws.monoid.all[NullArgument[Int, Int]]
}
