package scalaprops
package neko

import cats.Monad
import cats.laws.MonadLaws

object monad {

  def leftIdentity[F[_]: Monad, A: Gen, B](implicit G1: Gen[A => F[B]], E: Eq[F[B]]) =
    forAllNeko(MonadLaws[F].monadLeftIdentity[A, B] _)

  def rightIdentity[F[_]: Monad, A](implicit G1: Gen[F[A]], E: Eq[F[A]]) =
    forAllNeko(MonadLaws[F].monadRightIdentity[A] _)

  def kleisliLeftIdentity[F[_]: Monad, A: Gen, B](implicit G1: Gen[A => F[B]], E: Eq[F[B]]) =
    forAllNeko(MonadLaws[F].kleisliLeftIdentity[A, B] _)

  def kleisliRightIdentity[F[_]: Monad, A: Gen, B](implicit G1: Gen[A => F[B]], E: Eq[F[B]]) =
    forAllNeko(MonadLaws[F].kleisliRightIdentity[A, B] _)

  def mapFlatMapCoherence[F[_]: Monad, A: Cogen, B: Gen](implicit G1: Gen[F[A]], E: Eq[F[B]]) =
    forAllNeko(MonadLaws[F].mapFlatMapCoherence[A, B] _)

  def laws[F[_]: Monad](implicit G1: Gen[F[Byte]], E: Eq[F[Byte]]) =
    Properties.properties(Neko.monad)(
      Neko.monadLeftIdentity -> leftIdentity[F, Byte, Byte],
      Neko.monadRightIdentity -> rightIdentity[F, Byte],
      Neko.monadKleisliLeftIdentity -> kleisliLeftIdentity[F, Byte, Byte],
      Neko.monadKleisliRightIdentity -> kleisliRightIdentity[F, Byte, Byte],
      Neko.monadLeftIdentity -> mapFlatMapCoherence[F, Byte, Byte]
    )

  def all[F[_]: Monad](implicit G1: Gen[F[Byte]], G2: Gen[F[Byte => Byte]], E: Eq[F[Byte]]) =
    Properties.fromProps(Neko.monadAll, laws[F], applicative.all[F], flatmap.all[F])
}
