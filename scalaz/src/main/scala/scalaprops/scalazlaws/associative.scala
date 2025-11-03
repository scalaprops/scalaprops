package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz.*

object associative {
  def leftRight[=>:[_, _], X, Y, Z](implicit
    F: Associative[=>:],
    af: Gen[X =>: (Y =>: Z)],
    ef: Equal[X =>: (Y =>: Z)]
  ) =
    forAll(F.associativeLaw.leftRight[X, Y, Z] _)

  def rightLeft[=>:[_, _], X, Y, Z](implicit
    F: Associative[=>:],
    af: Gen[(X =>: Y) =>: Z],
    ef: Equal[(X =>: Y) =>: Z]
  ) =
    forAll(F.associativeLaw.rightLeft[X, Y, Z] _)

  def laws[=>:[_, _]](implicit
    F: Associative[=>:],
    al: Gen[(Int =>: Int) =>: Int],
    ar: Gen[Int =>: (Int =>: Int)],
    el: Equal[(Int =>: Int) =>: Int],
    er: Equal[Int =>: (Int =>: Int)]
  ) =
    Properties.properties(ScalazLaw.associative)(
      ScalazLaw.associativeLeftRight -> leftRight[=>:, Int, Int, Int],
      ScalazLaw.associativeRightLeft -> rightLeft[=>:, Int, Int, Int]
    )

  def all[=>:[_, _]](implicit
    F: Associative[=>:],
    al: Gen[(Int =>: Int) =>: Int],
    ar: Gen[Int =>: (Int =>: Int)],
    el: Equal[(Int =>: Int) =>: Int],
    er: Equal[Int =>: (Int =>: Int)]
  ) =
    laws[=>:]
}
