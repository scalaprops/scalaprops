package scalaprops
package scalazlaws

import scalaprops.Properties.properties
import scalaprops.Property.forAll
import scalaz.*

object equal {
  def commutativity[A](implicit A: Equal[A], G: Gen[A]) =
    forAll(A.equalLaw.commutative _)

  def reflexive[A](implicit A: Equal[A], G: Gen[A]) =
    forAll(A.equalLaw.reflexive _)

  def transitive[A](implicit A: Equal[A], G: Gen[A]) =
    forAll(A.equalLaw.transitive _)

  def naturality[A](implicit A: Equal[A], G: Gen[A]) =
    forAll(A.equalLaw.naturality _)

  def laws[A](implicit A: Equal[A], G: Gen[A]): Properties[ScalazLaw] =
    properties(ScalazLaw.equal)(
      ScalazLaw.equalCommutativity -> commutativity[A],
      ScalazLaw.equalReflexive -> reflexive[A],
      ScalazLaw.equalTransitive -> transitive[A],
      ScalazLaw.equalNaturality -> naturality[A]
    )

  def all[A](implicit A: Equal[A], G: Gen[A]): Properties[ScalazLaw] =
    laws[A]
}
