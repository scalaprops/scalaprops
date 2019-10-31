package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaprops.Properties.properties
import scalaz._

object plus {
  def associative[F[_], X](implicit f: Plus[F], afx: Gen[F[X]], ef: Equal[F[X]]) =
    forAll(f.plusLaw.associative[X] _)

  def laws[F[_]: Plus](implicit afx: Gen[F[Int]], ef: Equal[F[Int]]): Properties[ScalazLaw] =
    properties(ScalazLaw.plus)(
      ScalazLaw.plusAssociative -> associative[F, Int]
    )

  def all[F[_]](implicit F: Plus[F], afx: Gen[F[Int]], ef: Equal[F[Int]]): Properties[ScalazLaw] = {
    implicit val s = F.semigroup[Int]
    Properties.fromProps(ScalazLaw.plusAll, plus.laws[F], semigroup.laws[F[Int]])
  }
}
