package scalaprops
package neko

import cats.functor.Invariant
import cats.laws.InvariantLaws

object invariant {

  def identity[F[_]: Invariant, A](implicit F: Gen[F[A]], E: Eq[F[A]]) =
    forAllNeko(InvariantLaws[F].invariantIdentity[A] _)

  def composition[F[_]: Invariant, A: Gen: Cogen, B: Gen: Cogen, C: Gen: Cogen](implicit
    G: Gen[F[A]], E1: Eq[F[A]], E2: Eq[F[C]]
  ) = forAllNeko(InvariantLaws[F].invariantComposition[A, B, C] _)

  def laws[F[_]: Invariant](implicit G: Gen[F[Byte]], E: Eq[F[Byte]]) =
    Properties.properties(Neko.invariant)(
      Neko.invariantIdentity -> identity[F, Byte],
      Neko.invariantComposition -> composition[F, Byte, Byte, Byte]
    )

  def all[F[_]: Invariant](implicit G: Gen[F[Byte]], E: Eq[F[Byte]]) =
    laws[F]

}
