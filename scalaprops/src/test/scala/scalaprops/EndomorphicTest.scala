package scalaprops

import scalaz._
import scalaz.std.anyVal._
import KleisliTest.kleisliEqual
import CokleisliTest.cokleisliEqual

object EndomorphicTest extends Scalaprops {

  override val param: Param = super.param.copy(maxSize = 10)

  private[this] implicit def endomorphicKleisliEqual[F[_], A](implicit
    F: Equal[Kleisli[F, A, A]]
  ): Equal[Endomorphic[({type l[a, b] = Kleisli[F, a, b]})#l, A]] =
    F.contramap(_.run)

  private[this] implicit def endomorphicCokleisliEqual[F[_], A](implicit
    F: Equal[Cokleisli[F, A, A]]
  ): Equal[Endomorphic[({type l[a, b] = Cokleisli[F, a, b]})#l, A]] =
    F.contramap(_.run)

  val testKleisliMaybe =
    scalazlaws.monoid.all[Endomorphic[({type l[a, b] = Kleisli[Maybe, a, b]})#l, Int]]

  val testKleisliIList =
    scalazlaws.monoid.all[Endomorphic[({type l[a, b] = Kleisli[IList, a, b]})#l, Int]]

  val testCokleisliMaybe =
    scalazlaws.semigroup.all[Endomorphic[({type l[a, b] = Cokleisli[Maybe, a, b]})#l, Int]]

  val testCokleisliNel =
    scalazlaws.monoid.all[Endomorphic[({type l[a, b] = Cokleisli[NonEmptyList, a, b]})#l, Int]]

}
