package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaprops.Properties.properties
import scalaz._

sealed abstract class semigroup[G[_]] {
  def associative[A: Equal: G](implicit A: Semigroup[A]): Property

  final def laws[A: Semigroup: Equal: G]: Properties[ScalazLaw] =
    properties(ScalazLaw.semigroup) (
      ScalazLaw.semigroupAssociative -> associative[A]
    )

  final def all[A: Semigroup: Equal: G]: Properties[ScalazLaw] =
    laws[A]
}

object semigroup extends semigroup[Gen] {
  def apply[G[_]](implicit F: semigroup[G]): semigroup[G] = F

  implicit val instance: semigroup[Gen] = this

  def associative[A: Equal: Gen](implicit A: Semigroup[A]): Property =
    forAll(A.semigroupLaw.associative _)
}
