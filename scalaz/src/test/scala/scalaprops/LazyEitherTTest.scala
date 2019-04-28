package scalaprops

import scalaz._
import scalaz.std.anyVal._
import LazyEitherTest.lazyEitherEqual
import ScalapropsScalaz._

object LazyEitherTTest extends Scalaprops {

  private[this] implicit def lazyEitherTEqual[F[_], A, B](implicit F: Equal[F[LazyEither[A, B]]]): Equal[LazyEitherT[F, A, B]] =
    F.contramap(_.run)

  val maybe1 = {
    type E = Int
    type F[A] = LazyEitherT[Maybe, E, A]
    Properties.list(
      scalazlaws.monadError.all[F, E],
      scalazlaws.monadPlus.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.traverse.all[F]
    )
  }

  val maybe2 =
    scalazlaws.bitraverse.all[({type l[a, b] = LazyEitherT[Maybe, a, b]})#l]

  val bindRecIList =
    scalazlaws.bindRec.all[({type l[a] = LazyEitherT[IList, Byte, a]})#l].andThenParam(Param.maxSize(1))

  val ilist1 = {
    type E = Int
    type F[A] = LazyEitherT[IList, E, A]
    Properties.list(
      scalazlaws.monadError.all[F, E],
      scalazlaws.monadPlus.all[F],
      scalazlaws.traverse.all[F]
    )
  }

  val ilist2 =
    scalazlaws.bitraverse.all[({type l[a, b] = LazyEitherT[IList, a, b]})#l]

  val nel = {
    type E = Int
    type F[A] = LazyEitherT[NonEmptyList, E, A]
    Properties.list(
      scalazlaws.monadError.all[F, E],
      scalazlaws.monadPlus.all[F],
      scalazlaws.traverse.all[F]
    )
  }

  val monadTrans = scalazlaws.monadTrans.all[({type l[f[_], a] = LazyEitherT[f, Int, a]})#l]
}
