package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz.*

object zip {
  def zipPreservation[F[_], X](implicit F: Zip[F], FF: Functor[F], afx: Gen[F[X]], ef: Equal[F[X]]) =
    forAll(F.zipLaw.zipPreservation[X] _)

  def zipSymmetric[F[_], X, Y](implicit F: Zip[F], FF: Functor[F], afx: Gen[F[X]], afy: Gen[F[Y]], ef: Equal[F[X]]) =
    forAll(F.zipLaw.zipSymmetric[X, Y] _)

  def zipApply[F[_]: Functor, X, Y, Z](implicit
    F: Zip[F],
    afx: Gen[F[X]],
    au: Gen[F[Y => Z]],
    av: Gen[F[X => Y]],
    e: Equal[F[Z]]
  ) =
    forAll(F.ap.applyLaw.composition[X, Y, Z] _)

  def laws[F[_]](implicit fa: Gen[F[Int]], F: Zip[F], FF: Functor[F], EF: Equal[F[Int]], FI: Gen[F[Int => Int]]) =
    Properties.fromChecks(ScalazLaw.zip)(
      ScalazLaw.zipPreservation -> Check(
        zipPreservation[F, Int]
      ),
      ScalazLaw.zipSymmetric -> Check(
        zipSymmetric[F, Int, Int]
      ),
      ScalazLaw.zipApply -> Check(
        zipApply[F, Int, Int, Int],
        Param.maxSize(5)
      )
    )

  def all[F[_]](implicit fa: Gen[F[Int]], F: Zip[F], FF: Functor[F], EF: Equal[F[Int]], FI: Gen[F[Int => Int]]) =
    laws[F]
}
