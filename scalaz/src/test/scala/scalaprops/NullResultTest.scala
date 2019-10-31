package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.option._
import scalaz.std.tuple._
import ScalapropsScalaz._

object NullResultTest extends Scalaprops {
  import FunctionEqual._

  private[this] implicit def equal[A, B](implicit A: Equal[A => Option[B]]): Equal[NullResult[A, B]] =
    A.contramap(_.apply)

  val arrow = scalazlaws.arrow.all[NullResult]
  val monadPlus = scalazlaws.monadPlusStrong.all[({ type l[a] = NullResult[Int, a] })#l]
  val testContravariant = scalazlaws.contravariant.all[({ type l[a] = NullResult[a, Int] })#l]
  val testMonoid = scalazlaws.monoid.all[NullResult[Int, Int]]
}
