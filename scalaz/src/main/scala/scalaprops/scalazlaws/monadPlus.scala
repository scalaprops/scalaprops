package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaprops.Properties.properties
import scalaz._

object monadPlus {
  def emptyMap[F[_], X](implicit f: MonadPlus[F], afx: Gen[X => X], ef: Equal[F[X]]): Property =
    forAll(f.monadPlusLaw.emptyMap[X] _)

  def leftZero[F[_], X](implicit F: MonadPlus[F], afx: Gen[X => F[X]], ef: Equal[F[X]]): Property =
    forAll(F.monadPlusLaw.leftZero[X] _)

  def laws[F[_]](implicit F: MonadPlus[F], afx: Gen[F[Int]], ef: Equal[F[Int]]): Properties[ScalazLaw] =
    properties(ScalazLaw.monadPlus)(
      ScalazLaw.monadPlusEmptyMap -> emptyMap[F, Int],
      ScalazLaw.monadPlusLeftZero -> leftZero[F, Int]
    )

  def all[F[_]](implicit
    F: MonadPlus[F],
    afx: Gen[F[Int]],
    afy: Gen[F[Int => Int]],
    ef: Equal[F[Int]]
  ): Properties[ScalazLaw] =
    Properties.fromProps(
      ScalazLaw.monadPlusAll,
      monadPlus.laws[F],
      monad.all[F],
      applicativePlus.all[F]
    )
}
