package scalaprops

import scalaz._
import scalaz.std.anyVal._

object NullArgumentTest extends Scalaprops {

  import FunctionEqual._

  private[this] implicit def equal[A, B](implicit A: Equal[Option[A] => B]): Equal[NullArgument[A, B]] =
    A.contramap(_.apply)

  val testCompose = scalazlaws.compose.all[NullArgument]
  val testMonad = scalazlaws.monad.all[({type l[a] = NullArgument[Int, a]})#l]
  val testContravariant = scalazlaws.contravariant.all[({type l[a] = NullArgument[a, Int]})#l]
  val testMonoid = scalazlaws.monoid.all[NullArgument[Int, Int]]
}
