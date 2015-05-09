package scalaprops

import scalaz.{Equal, LazyEither}
import scalaz.std.anyVal._
import scalaz.std.string._
import scalaz.std.either._

object LazyEitherTest extends Scalaprops {

  private[this] implicit def lazyEitherEqual[A: Equal, B: Equal]: Equal[LazyEither[A, B]] =
    Equal[A Either B].contramap(_.toEither)

  val testLaws1 = Properties.either(
    "LazyEither",
    scalazlaws.monadError.all[LazyEither, Int],
    scalazlaws.traverse.all[({type l[a] = LazyEither[Int, a]})#l]
  )

  val testLaw2 = scalazlaws.bitraverse.all[LazyEither]
  val testLaw3 = scalazlaws.associative.all[LazyEither]

}
