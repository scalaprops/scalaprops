package scalaprops
package scalazlaws

import scalaprops.Properties.properties
import scalaprops.Property.forAll
import scalaz.*

object semilattice {
  def commutative[A: Equal: Gen](implicit A: SemiLattice[A]): Property =
    forAll(A.semiLatticeLaw.commutative _)

  def laws[A: SemiLattice: Equal: Gen]: Properties[ScalazLaw] =
    properties(ScalazLaw.semilattice)(
      ScalazLaw.semilatticeCommutative -> commutative[A]
    )

  def all[A: SemiLattice: Equal: Gen]: Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.semilatticeAll, band.all[A], semilattice.laws[A])
}
