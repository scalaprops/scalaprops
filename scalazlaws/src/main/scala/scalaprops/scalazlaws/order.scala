package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaprops.Properties.properties
import scala.math.{Ordering => SOrdering}
import scalaz._

object order {
  def antisymmetric[A](implicit A: Order[A], G: Gen[A]): Property =
    forAll(A.orderLaw.antisymmetric _)

  def transitiveOrder[A](implicit A: Order[A], G: Gen[A]): Property =
    forAll(A.orderLaw.transitiveOrder _)

  def orderAndEqualConsistent[A](implicit A: Order[A], G: Gen[A]): Property =
    forAll(A.orderLaw.orderAndEqualConsistent _)

  def scalaOrdering[A: Order : SOrdering : Gen] =
    ScalazLaw.orderConsistentScalaOrdering -> forAll((a1: A, a2: A) => Order[A].order(a1, a2) == Ordering.fromInt(SOrdering[A].compare(a1, a2)))

  def laws[A: Order : Gen]: Properties[ScalazLaw] =
    properties(ScalazLaw.order)(
      ScalazLaw.orderAntisymmetric -> antisymmetric[A],
      ScalazLaw.orderTransitiveOrder -> transitiveOrder[A],
      ScalazLaw.orderOrderAndEqualConsistent -> orderAndEqualConsistent[A]
    )

  def all[A: Order : Gen]: Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.orderAll, order.laws[A], equal.laws[A])
}
