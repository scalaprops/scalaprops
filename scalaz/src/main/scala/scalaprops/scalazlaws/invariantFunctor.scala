package scalaprops
package scalazlaws

import scalaprops.Properties.properties
import scalaprops.Property.forAll
import scalaz.*

object invariantFunctor {
  def identity[F[_], X](implicit F: InvariantFunctor[F], afx: Gen[F[X]], ef: Equal[F[X]]): Property =
    forAll(F.invariantFunctorLaw.invariantIdentity[X] _)

  def composite[F[_], X, Y, Z](implicit
    F: InvariantFunctor[F],
    af: Gen[F[X]],
    axy: Gen[X => Y],
    ayz: Gen[Y => Z],
    ayx: Gen[Y => X],
    azy: Gen[Z => Y],
    ef: Equal[F[Z]]
  ): Property =
    forAll(F.invariantFunctorLaw.invariantComposite[X, Y, Z] _)

  def laws[F[_]](implicit
    F: InvariantFunctor[F],
    af: Gen[F[Int]],
    axy: Gen[Int => Int],
    ef: Equal[F[Int]]
  ): Properties[ScalazLaw] =
    properties(ScalazLaw.invariantFunctor)(
      ScalazLaw.invariantFunctorIdentity -> identity[F, Int],
      ScalazLaw.invariantFunctorComposite -> composite[F, Int, Int, Int]
    )

  def all[F[_]](implicit
    F: InvariantFunctor[F],
    af: Gen[F[Int]],
    axy: Gen[Int => Int],
    ef: Equal[F[Int]]
  ): Properties[ScalazLaw] =
    laws[F]
}
