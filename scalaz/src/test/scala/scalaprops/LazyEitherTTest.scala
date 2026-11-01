package scalaprops

import LazyEitherTest.lazyEitherEqual
import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object LazyEitherTTest extends Scalaprops {
  private[this] implicit def lazyEitherTEqual[F[_], A, B](implicit
    F: Equal[F[LazyEither[A, B]]]
  ): Equal[LazyEitherT[F, A, B]] =
    F.contramap(_.run)

  property("maybe1") = {
    type E = Int
    type F[A] = LazyEitherT[Maybe, E, A]
    Properties.list(
      scalazlaws.monadError.all[F, E],
      scalazlaws.monadPlus.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.traverse.all[F]
    )
  }

  property("maybe2") = scalazlaws.bitraverse.all[({ type l[a, b] = LazyEitherT[Maybe, a, b] })#l]

  property("bindRecIList") =
    scalazlaws.bindRec.all[({ type l[a] = LazyEitherT[IList, Byte, a] })#l].andThenParam(Param.maxSize(1))

  property("ilist1") = {
    type E = Int
    type F[A] = LazyEitherT[IList, E, A]
    Properties.list(
      scalazlaws.monadError.all[F, E],
      scalazlaws.monadPlus.all[F],
      scalazlaws.traverse.all[F]
    )
  }

  property("ilist2") = scalazlaws.bitraverse.all[({ type l[a, b] = LazyEitherT[IList, a, b] })#l]

  property("nel") = {
    type E = Int
    type F[A] = LazyEitherT[NonEmptyList, E, A]
    Properties.list(
      scalazlaws.monadError.all[F, E],
      scalazlaws.monadPlus.all[F],
      scalazlaws.traverse.all[F]
    )
  }

  property("monadTrans") = scalazlaws.monadTrans.all[({ type l[f[_], a] = LazyEitherT[f, Int, a] })#l]
}
