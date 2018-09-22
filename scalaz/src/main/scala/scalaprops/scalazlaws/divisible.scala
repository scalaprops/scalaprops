package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz._

object divisible {
  def rightIdentity[F[_], A](implicit F: Divisible[F], G: Gen[F[A]], E: Equal[F[A]]): Property =
    forAll(F.divisibleLaw.rightIdentity[A] _)

  def leftIdentity[F[_], A](implicit F: Divisible[F], G: Gen[F[A]], E: Equal[F[A]]): Property =
    forAll(F.divisibleLaw.leftIdentity[A] _)

  def laws[F[_]: Divisible](implicit
    G: Gen[F[Int]], E: Equal[F[Int]]
  ): Properties[ScalazLaw] =
    Properties.properties(ScalazLaw.divisible)(
      ScalazLaw.divisibleRightIdentity -> this.rightIdentity[F, Int],
      ScalazLaw.divisibleLeftIdentity -> this.leftIdentity[F, Int]
    )

  def all[F[_]: Divisible](implicit G: Gen[F[Int]], E: Equal[F[Int]]): Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.divisibleAll, divisible.laws[F], divide.all[F])
}
