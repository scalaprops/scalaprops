package scalaprops
package scalazlaws

import scalaz.{Equal, Representable}
import scalaz.std.anyVal._
import scalaprops.Property.forAll

object representable {
  def repUnrep[F[_], X, A](implicit F: Representable[F, X], G: Gen[F[A]], E: Equal[F[A]]): Property =
    forAll(F.representableLaw.repUnrep[A] _)

  def unrepRep[F[_], X, A](implicit F: Representable[F, X], G1: Gen[X => A], G2: Gen[X], E: Equal[A]): Property =
    forAll(F.representableLaw.unrepRep[A] _)

  def laws[F[_], X](
    implicit F: Representable[F, X],
    G1: Gen[X => Byte],
    G2: Gen[F[Byte]],
    G3: Gen[X],
    E: Equal[F[Byte]]
  ) =
    Properties.properties(ScalazLaw.representable)(
      ScalazLaw.representableRepUnrep -> repUnrep[F, X, Byte],
      ScalazLaw.representableUnrepRep -> unrepRep[F, X, Byte]
    )

  def all[F[_], X](
    implicit F: Representable[F, X],
    G1: Gen[X => Byte],
    G2: Gen[F[Byte]],
    G3: Gen[X],
    E: Equal[F[Byte]]
  ) =
    laws[F, X]
}
