package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
object NullArgumentTest extends Scalaprops {
  import FunctionEqual.*

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
