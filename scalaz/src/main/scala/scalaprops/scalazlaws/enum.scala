package scalaprops
package scalazlaws

import scalaprops.Properties.properties
import scalaprops.Property.forAll
import scalaprops.Property.forAllG
import scalaz.*

object `enum` {
  def succpred[A: Gen](implicit A: Enum[A]): Property =
    forAll(A.enumLaw.succpred _)

  def predsucc[A: Gen](implicit A: Enum[A]): Property =
    forAll(A.enumLaw.predsucc _)

  def minmaxpred[A](implicit A: Enum[A]): Property =
    forAll(A.enumLaw.minmaxpred)

  def minmaxsucc[A](implicit A: Enum[A]): Property =
    forAll(A.enumLaw.minmaxsucc)

  private[this] val smallInt = Gen.choose(-100, 100)

  def succn[A](implicit A: Enum[A], G: Gen[A]): Property =
    forAllG(G, smallInt)(A.enumLaw.succn)

  def predn[A](implicit A: Enum[A], G: Gen[A]): Property =
    forAllG(G, smallInt)(A.enumLaw.predn)

  def succorder[A](implicit A: Enum[A], G: Gen[A]): Property =
    forAll(A.enumLaw.succorder _)

  def predorder[A](implicit A: Enum[A], G: Gen[A]): Property =
    forAll(A.enumLaw.predorder _)

  def laws[A: Enum: Gen]: Properties[ScalazLaw] =
    properties(ScalazLaw.`enum`)(
      ScalazLaw.enumSuccPred -> succpred[A],
      ScalazLaw.enumPredSucc -> predsucc[A],
      ScalazLaw.enumMinMaxPred -> minmaxpred[A],
      ScalazLaw.enumMinMaxSucc -> minmaxsucc[A],
      ScalazLaw.enumSuccN -> succn[A],
      ScalazLaw.enumPredN -> predn[A],
      ScalazLaw.enumSuccOrder -> succorder[A],
      ScalazLaw.enumPredOrder -> predorder[A]
    )

  def all[A: Enum: Gen]: Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.enumAll, `enum`.laws[A], order.all[A])
}
