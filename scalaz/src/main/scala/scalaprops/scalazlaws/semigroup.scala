package scalaprops
package scalazlaws

import scalaprops.Properties.properties
import scalaprops.Property.forAll
import scalaz.*

object semigroup {
  def associative[A: Equal: Gen](implicit A: Semigroup[A]): Property =
    forAll(A.semigroupLaw.associative _)

  def laws[A: Semigroup: Equal: Gen]: Properties[ScalazLaw] =
    properties(ScalazLaw.semigroup)(
      ScalazLaw.semigroupAssociative -> associative[A]
    )

  def all[A: Semigroup: Equal: Gen]: Properties[ScalazLaw] =
    laws[A]
}
