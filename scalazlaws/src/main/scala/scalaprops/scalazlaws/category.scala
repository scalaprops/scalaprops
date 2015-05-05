package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz._

object category {
  def leftIdentity[=>:[_, _], A, B](implicit ab: Gen[A =>: B], C: Category[=>:], E: Equal[A =>: B]) =
    forAll(C.categoryLaw.leftIdentity[A, B] _)

  def rightIdentity[=>:[_, _], A, B](implicit ab: Gen[A =>: B], C: Category[=>:], E: Equal[A =>: B]) =
    forAll(C.categoryLaw.rightIdentity[A, B] _)

  def laws[=>:[_, _]](implicit C: Category[=>:], AB: Gen[Int =>: Int], E: Equal[Int =>: Int]) =
    Properties.properties(ScalazLaw.category) (
      ScalazLaw.categoryLeftIdentity -> leftIdentity[=>:, Int, Int],
      ScalazLaw.categoryRightIdentity -> rightIdentity[=>:, Int, Int]
    )

  def all[=>:[_, _]](implicit C: Category[=>:], AB: Gen[Int =>: Int], E: Equal[Int =>: Int]) = {
    implicit val p = C.empty
    implicit val s = C.monoid[Int]

    Properties.fromProps(
      ScalazLaw.categoryAll,
      category.laws[=>:],
      compose.all[=>:],
      plusEmpty.all[({type l[a] = a =>: a})#l],
      monoid.all[Int =>: Int]
    )
  }
}
