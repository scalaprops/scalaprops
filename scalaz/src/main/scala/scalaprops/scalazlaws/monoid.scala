package scalaprops
package scalazlaws

import scalaprops.Properties.properties
import scalaprops.Property.forAll
import scalaz.*

object monoid {
  def leftIdentity[A: Equal: Gen](implicit A: Monoid[A]): Property =
    forAll(A.monoidLaw.leftIdentity _)

  def rightIdentity[A: Equal: Gen](implicit A: Monoid[A]): Property =
    forAll(A.monoidLaw.rightIdentity _)

  def laws[A: Monoid: Equal: Gen]: Properties[ScalazLaw] =
    properties(ScalazLaw.monoid)(
      ScalazLaw.monoidLeftIdentity -> leftIdentity[A],
      ScalazLaw.monoidRightIdentity -> rightIdentity[A]
    )

  def all[A: Monoid: Equal: Gen]: Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.monoidAll, semigroup.laws[A], monoid.laws[A])
}
