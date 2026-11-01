package scalaprops

import CokleisliTest.cokleisliEqual
import KleisliTest.kleisliEqual
import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object EndomorphicTest extends Scalaprops {
  override val param: Param = super.param.copy(maxSize = 10)

  private[this] implicit def endomorphicKleisliEqual[F[_], A](implicit
    F: Equal[Kleisli[F, A, A]]
  ): Equal[Endomorphic[({ type l[a, b] = Kleisli[F, a, b] })#l, A]] =
    F.contramap(_.run)

  private[this] implicit def endomorphicCokleisliEqual[F[_], A](implicit
    F: Equal[Cokleisli[F, A, A]]
  ): Equal[Endomorphic[({ type l[a, b] = Cokleisli[F, a, b] })#l, A]] =
    F.contramap(_.run)

  property("kleisliMaybe") = scalazlaws.monoid.all[Endomorphic[({ type l[a, b] = Kleisli[Maybe, a, b] })#l, Int]]

  property("kleisliIList") = scalazlaws.monoid.all[Endomorphic[({ type l[a, b] = Kleisli[IList, a, b] })#l, Int]]

  property("cokleisliMaybe") = scalazlaws.semigroup.all[Endomorphic[({ type l[a, b] = Cokleisli[Maybe, a, b] })#l, Int]]

  property("cokleisliNel") =
    scalazlaws.monoid.all[Endomorphic[({ type l[a, b] = Cokleisli[NonEmptyList, a, b] })#l, Int]]
}
