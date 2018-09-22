package scalaprops
package scalazlaws

import scalaz._
import scalaprops.Property.forAll

object lens {

  def identity[A: Gen: Equal, B](l: Lens[A, B]) =
    forAll(l.lensLaw.identity _)
  def retention[A: Gen, B: Gen: Equal](l: Lens[A, B]) =
    forAll(l.lensLaw.retention _)
  def doubleSet[A: Gen: Equal, B: Gen](l: Lens[A, B]) =
    forAll(l.lensLaw.doubleSet _)

  def laws[A: Gen: Equal, B: Gen: Equal](l: Lens[A, B]) =
    Properties.properties(ScalazLaw.lens)(
      ScalazLaw.lensIdentity -> identity(l),
      ScalazLaw.lensRetention -> retention(l),
      ScalazLaw.lensDoubleSet -> doubleSet(l)
    )

  def all[A: Gen: Equal, B: Gen: Equal](l: Lens[A, B]) =
    laws(l)
}
