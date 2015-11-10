package scalaprops
package neko

import cats.Applicative
import cats.laws.ApplicativeLaws

object applicative {

  def identity[F[_]: Applicative, A](implicit F: Gen[F[A]], E: Eq[F[A]]) =
    forAllNeko(ApplicativeLaws[F].applicativeIdentity[A] _)

  def homomorphism[F[_]: Applicative, A: Gen: Cogen, B: Gen](implicit E: Eq[F[B]]) =
    forAllNeko(ApplicativeLaws[F].applicativeHomomorphism[A, B] _)

  def interchange[F[_]: Applicative, A: Gen, B](implicit G: Gen[F[A => B]], E: Eq[F[B]]) =
    forAllNeko(ApplicativeLaws[F].applicativeInterchange[A, B] _)

  def map[F[_]: Applicative, A: Cogen, B: Gen](implicit G: Gen[F[A]], E: Eq[F[B]]) =
    forAllNeko(ApplicativeLaws[F].applicativeMap[A, B] _)

  def laws[F[_]: Applicative](implicit G1: Gen[F[Byte]], G2: Gen[F[Byte => Byte]], E1: Eq[F[Byte]]) =
    Properties.properties(Neko.applicative)(
      Neko.applicativeIdentity -> identity[F, Byte],
      Neko.applicativeHomomorphism -> homomorphism[F, Byte, Byte],
      Neko.applicativeInterchange -> interchange[F, Byte, Byte],
      Neko.applicativeMap -> map[F, Byte, Byte]
    )

  def all[F[_]: Applicative](implicit G1: Gen[F[Byte]], G2: Gen[F[Byte => Byte]], E1: Eq[F[Byte]]) =
    Properties.fromProps(Neko.applicativeAll, laws[F], apply.all[F])
}
