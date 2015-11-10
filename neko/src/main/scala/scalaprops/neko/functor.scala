package scalaprops
package neko

import cats.Functor
import cats.laws.FunctorLaws

object functor {

  def identity[F[_]: Functor, A](implicit F: Gen[F[A]], E: Eq[F[A]]) =
    forAllNeko(FunctorLaws[F].covariantIdentity[A] _)

  def composition[F[_]: Functor, A: Cogen, B: Gen: Cogen, C: Gen](implicit F: Gen[F[A]], E: Eq[F[C]]) =
    forAllNeko(FunctorLaws[F].covariantComposition[A, B, C] _)

  def laws[F[_]: Functor](implicit G: Gen[F[Byte]], E: Eq[F[Byte]]) =
    Properties.properties(Neko.functor)(
      Neko.functorIdentity -> identity[F, Byte],
      Neko.functorComposition -> composition[F, Byte, Byte, Byte]
    )

  def all[F[_]: Functor](implicit G: Gen[F[Byte]], E: Eq[F[Byte]]) =
    Properties.fromProps(Neko.functorAll, invariant.all[F], laws[F])

}
