package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaprops.Properties.properties
import scalaz._

object functor {

  def identity[F[_], X](implicit F: Functor[F], afx: Gen[F[X]], ef: Equal[F[X]]) =
    forAll(F.functorLaw.identity[X] _)

  def composite[F[_], X, Y, Z](implicit F: Functor[F], af: Gen[F[X]], axy: Gen[X => Y],
                               ayz: Gen[Y => Z], ef: Equal[F[Z]]) =
    forAll(F.functorLaw.composite[X, Y, Z] _)

  def laws[F[_]](implicit F: Functor[F], af: Gen[F[Int]], axy: Gen[Int => Int],
                 ef: Equal[F[Int]]) =
    properties(ScalazLaw.functor)(
      ScalazLaw.functorIdentity -> identity[F, Int],
      ScalazLaw.functorCompsite -> composite[F, Int, Int, Int]
    )

  def all[F[_]](implicit F: Functor[F], af: Gen[F[Int]], axy: Gen[Int => Int],
                ef: Equal[F[Int]]) =
    Properties.fromProps(ScalazLaw.functorAll, functor.laws[F], invariantFunctor.laws[F])
}
