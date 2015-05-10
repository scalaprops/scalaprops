package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._
import LazyEitherTest.lazyEitherEqual

object LazyEitherTTest extends Scalaprops {

  private[this] implicit def lazyEitherTEqual[F[_], A, B](implicit F: Equal[F[LazyEither[A, B]]]): Equal[LazyEitherT[F, A, B]] =
    F.contramap(_.run)

  val maybe1 =
    Properties.either(
      "LazyEitherT[Maybe, _, _]",
      scalazlaws.monadError.all[({type l[a, b] = LazyEitherT[Maybe, a, b]})#l, Int],
      scalazlaws.traverse.all[({type l[a] = LazyEitherT[Maybe, Int, a]})#l]
    )

  val maybe2 =
    scalazlaws.bitraverse.all[({type l[a, b] = LazyEitherT[Maybe, a, b]})#l]

  val ilist1 =
    Properties.either(
      "LazyEitherT[IList, _, _]",
      scalazlaws.monadError.all[({type l[a, b] = LazyEitherT[IList, a, b]})#l, Int],
      scalazlaws.traverse.all[({type l[a] = LazyEitherT[IList, Int, a]})#l]
    )

  val ilist2 =
    scalazlaws.bitraverse.all[({type l[a, b] = LazyEitherT[IList, a, b]})#l]

  val nel =
    Properties.either(
      "LazyEitherT[NonEmptyList, _, _]",
      scalazlaws.monadError.all[({type l[a, b] = LazyEitherT[NonEmptyList, a, b]})#l, Int],
      scalazlaws.traverse.all[({type l[a] = LazyEitherT[NonEmptyList, Int, a]})#l]
    )

}
