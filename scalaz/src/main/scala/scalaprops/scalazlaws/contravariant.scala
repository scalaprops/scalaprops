package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz._

object contravariant {
  def identity[F[_], X](implicit F: Contravariant[F], afx: Gen[F[X]], ef: Equal[F[X]]) =
    forAll(F.contravariantLaw.identity[X] _)

  def composite[F[_], X, Y, Z](
    implicit F: Contravariant[F],
    af: Gen[F[Z]],
    axy: Gen[X => Y],
    ayz: Gen[Y => Z],
    ef: Equal[F[X]]
  ) =
    forAll(F.contravariantLaw.composite[Z, Y, X] _)

  def laws[F[_]](implicit F: Contravariant[F], af: Gen[F[Int]], axy: Gen[Int => Int], ef: Equal[F[Int]]) =
    Properties.properties(ScalazLaw.contravariant)(
      ScalazLaw.contravariantIdentity -> identity[F, Int],
      ScalazLaw.contravariantComposite -> composite[F, Int, Int, Int]
    )

  def all[F[_]](implicit F: Contravariant[F], af: Gen[F[Int]], axy: Gen[Int => Int], ef: Equal[F[Int]]) =
    Properties.fromProps(ScalazLaw.contravariantAll, laws[F], invariantFunctor.all[F])
}
