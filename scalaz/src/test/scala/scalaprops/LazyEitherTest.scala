package scalaprops

import ScalapropsScalaz.*
import scalaz.Equal
import scalaz.LazyEither
import scalaz.std.anyVal.*
import scalaz.std.either.*

object LazyEitherTest extends Scalaprops {
  implicit def lazyEitherEqual[A: Equal, B: Equal]: Equal[LazyEither[A, B]] =
    Equal[A Either B].contramap(_.toEither)

  property("laws1") = Properties.list(
    scalazlaws.monadError.all[({ type l[a] = LazyEither[Int, a] })#l, Int],
    scalazlaws.bindRec.all[({ type l[a] = LazyEither[Int, a] })#l],
    scalazlaws.traverse.all[({ type l[a] = LazyEither[Int, a] })#l]
  )

  property("law2") = scalazlaws.bitraverse.all[LazyEither]
  property("law3") = scalazlaws.associative.all[LazyEither]
}
