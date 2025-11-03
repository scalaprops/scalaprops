package scalaprops
package scalazlaws

import scalaprops.Properties.properties
import scalaprops.Property.forAll
import scalaz.*

object band {
  def idempotency[A: Equal: Gen](implicit A: Band[A]): Property =
    forAll(A.bandLaw.idempotency _)

  def laws[A: Band: Equal: Gen]: Properties[ScalazLaw] =
    properties(ScalazLaw.band)(
      ScalazLaw.bandIdempotency -> idempotency[A]
    )

  def all[A: Band: Equal: Gen]: Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.bandAll, semigroup.laws[A], band.laws[A])
}
