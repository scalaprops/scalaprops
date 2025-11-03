package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz.Equal
import scalaz.Isomorphism.*

object iso {
  def a2b[A: Gen, B](i: A <=> B)(implicit A: Equal[A]): Property =
    forAll { (a: A) => A.equal(i.from(i.to(a)), a) }

  def b2a[A, B: Gen](i: A <=> B)(implicit B: Equal[B]): Property =
    forAll { (b: B) => B.equal(i.to(i.from(b)), b) }

  def all[A: Gen: Equal, B: Gen: Equal](i: A <=> B): Properties[ScalazLaw] =
    Properties.properties(ScalazLaw.iso)(
      ScalazLaw.isoA2B -> a2b(i),
      ScalazLaw.isoB2A -> b2a(i)
    )
}
