package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*
import scalaz.std.option.*
import scalaz.std.tuple.*

object NullResultTest extends Scalaprops {
  import FunctionEqual.*

  private[this] implicit def equal[A, B](implicit A: Equal[A => Option[B]]): Equal[NullResult[A, B]] =
    A.contramap(_.apply)

  property("arrow") = scalazlaws.arrow.all[NullResult]
  property("monadPlus") = scalazlaws.monadPlusStrong.all[({ type l[a] = NullResult[Int, a] })#l]
  property("contravariant") = scalazlaws.contravariant.all[({ type l[a] = NullResult[a, Int] })#l]
  property("monoid") = scalazlaws.monoid.all[NullResult[Int, Int]]
}
