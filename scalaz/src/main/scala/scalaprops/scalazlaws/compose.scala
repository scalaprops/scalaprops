package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaprops.Properties.properties
import scalaz._

object compose {
  def associative[=>:[_, _], A, B, C, D](implicit ab: Gen[A =>: B], bc: Gen[B =>: C],
                                         cd: Gen[C =>: D], C: Compose[=>:], E: Equal[A =>: D]): Property =
    forAll(C.composeLaw.associative[A, B, C, D] _)

  def laws[=>:[_, _]](implicit C: Compose[=>:], AB: Gen[Int =>: Int], E: Equal[Int =>: Int]): Properties[ScalazLaw] =
    properties(ScalazLaw.compose)(
      ScalazLaw.composeAssociative -> associative[=>:, Int, Int, Int, Int]
    )

  def all[=>:[_, _]](implicit C: Compose[=>:], AB: Gen[Int =>: Int], E: Equal[Int =>: Int]): Properties[ScalazLaw] = {
    implicit val p = C.plus
    implicit val s = C.semigroup[Int]

    Properties.fromProps(
      ScalazLaw.composeAll,
      compose.laws[=>:],
      plus.all[({type l[a] = a =>: a})#l],
      semigroup.all[Int =>: Int]
    )
  }
}
