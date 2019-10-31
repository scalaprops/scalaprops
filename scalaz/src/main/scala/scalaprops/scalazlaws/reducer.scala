package scalaprops
package scalazlaws

import scalaz._
import scalaprops.Property.forAll

object reducer {
  def consCorrectness[A: Gen, B: Gen: Equal](implicit R: Reducer[A, B]) =
    forAll(R.reducerLaw.consCorrectness _)
  def snocCorrectness[A: Gen, B: Gen: Equal](implicit R: Reducer[A, B]) =
    forAll(R.reducerLaw.snocCorrectness _)

  def laws[A: Gen, B: Gen: Equal](implicit R: Reducer[A, B]) =
    Properties.properties(ScalazLaw.reducer)(
      ScalazLaw.reducerConsCorrectness -> consCorrectness[A, B],
      ScalazLaw.reducerSnocCorrectness -> snocCorrectness[A, B]
    )

  def all[A: Gen, B: Gen: Equal](implicit R: Reducer[A, B]) =
    laws[A, B]
}
