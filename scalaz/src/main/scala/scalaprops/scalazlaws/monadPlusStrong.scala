package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaprops.Properties.properties
import scalaz.{Equal, MonadPlus}

object monadPlusStrong {
  def rightZero[F[_], X](implicit F: MonadPlus[F], afx: Gen[F[X]], ef: Equal[F[X]]): Property =
    forAll(F.strongMonadPlusLaw.rightZero[X] _)

  def laws[F[_]](implicit F: MonadPlus[F], afx: Gen[F[Int]], ef: Equal[F[Int]]): Properties[ScalazLaw] =
    properties(ScalazLaw.monadPlusStrong)(
      ScalazLaw.monadPlusRightZero -> rightZero[F, Int]
    )

  def all[F[_]](implicit
    F: MonadPlus[F],
    afx: Gen[F[Int]],
    afy: Gen[F[Int => Int]],
    ef: Equal[F[Int]]
  ): Properties[ScalazLaw] =
    Properties.fromProps(
      ScalazLaw.monadPlusStrongAll,
      monadPlusStrong.laws[F],
      monadPlus.all[F]
    )
}
