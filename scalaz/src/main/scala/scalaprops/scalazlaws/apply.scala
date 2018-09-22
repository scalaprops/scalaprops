package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz._

object apply {
  def composition[F[_], X, Y, Z](implicit ap: Apply[F], afx: Gen[F[X]], au: Gen[F[Y => Z]],
                                 av: Gen[F[X => Y]], e: Equal[F[Z]]) =
    forAll(ap.applyLaw.composition[X, Y, Z] _)

  def laws[F[_]](implicit F: Apply[F], af: Gen[F[Int]],
                 aff: Gen[F[Int => Int]], e: Equal[F[Int]]): Properties[ScalazLaw] =
    Properties.fromChecks(ScalazLaw.apply)(
      ScalazLaw.applyComposition -> Check(
        composition[F, Int, Int, Int], Param.maxSize(5)
      )
    )

  def all[F[_]](implicit F: Apply[F], af: Gen[F[Int]],
                aff: Gen[F[Int => Int]], e: Equal[F[Int]]): Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.applyAll, apply.laws[F], functor.all[F])
}
