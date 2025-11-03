package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*
import scalaz.std.option.*
import scalaz.std.tuple.*

@scalajs.js.annotation.JSExportAll
object NullResultTest extends Scalaprops {
  import FunctionEqual.*

  private[this] implicit def equal[A, B](implicit A: Equal[A => Option[B]]): Equal[NullResult[A, B]] =
    A.contramap(_.apply)

  val arrow = scalazlaws.arrow.all[NullResult]
  val monadPlus = scalazlaws.monadPlusStrong.all[({ type l[a] = NullResult[Int, a] })#l]
  val testContravariant = scalazlaws.contravariant.all[({ type l[a] = NullResult[a, Int] })#l]
  val testMonoid = scalazlaws.monoid.all[NullResult[Int, Int]]
}
