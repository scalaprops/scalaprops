package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz._

object divide {
  def composition[F[_], A](implicit F: Divide[F], G: Gen[F[A]], E: Equal[F[A]]): Property =
    forAll(F.divideLaw.composition[A] _)

  def laws[F[_]: Divide](
    implicit
    G: Gen[F[Int]],
    E: Equal[F[Int]]
  ): Properties[ScalazLaw] =
    Properties.properties(ScalazLaw.divide)(
      ScalazLaw.divideComposition -> this.composition[F, Int]
    )

  def all[F[_]: Divide](implicit G: Gen[F[Int]], E: Equal[F[Int]]): Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.divideAll, divide.laws[F], contravariant.all[F])
}
