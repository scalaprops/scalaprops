package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaprops.Properties.properties
import scalaz._

sealed abstract class plus[G[_], A](
  implicit A1: semigroup[G]
) {

  def associative[M[_], X](implicit f: Plus[M], afx: G[M[X]], ef: Equal[M[X]]): Property

  final def laws[M[_]: Plus](implicit afx: G[M[A]], ef: Equal[M[A]]): Properties[ScalazLaw] =
    properties(ScalazLaw.plus)(
      ScalazLaw.plusAssociative -> associative[M, A]
    )

  final def all[M[_]](implicit F: Plus[M], afx: G[M[A]], ef: Equal[M[A]]): Properties[ScalazLaw] = {
    implicit val s = F.semigroup[A]
    Properties.fromProps(ScalazLaw.plusAll, this.laws[M], semigroup[G].laws[M[A]])
  }

}

object plus extends plus[Gen, Int] {
  def apply[F[_, _], G[_], A](implicit A: plus[G, A]): plus[G, A] = A

  implicit val int: plus[Gen, Int] = this

  def associative[M[_], X](implicit f: Plus[M], afx: Gen[M[X]], ef: Equal[M[X]]) =
    forAll(f.plusLaw.associative[X] _)

}
