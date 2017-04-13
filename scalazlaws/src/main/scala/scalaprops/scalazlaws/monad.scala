package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaprops.Properties.properties
import scalaz._

object monad {
  def rightIdentity[M[_], X](implicit M: Monad[M], e: Equal[M[X]], a: Gen[M[X]]) =
    forAll(M.monadLaw.rightIdentity[X] _)

  def leftIdentity[M[_], X, Y](implicit am: Monad[M], emy: Equal[M[Y]], ax: Gen[X], af: Gen[X => M[Y]]) =
    forAll(am.monadLaw.leftIdentity[X, Y] _)

  def laws[M[_]](implicit a: Monad[M], am: Gen[M[Int]],
                 af: Gen[Int => M[Int]], e: Equal[M[Int]]) =
    properties(ScalazLaw.monad)(
      ScalazLaw.monadRightIdentity -> monad.rightIdentity[M, Int],
      ScalazLaw.monadLeftIdentity -> monad.leftIdentity[M, Int, Int]
    )

  def all[M[_]](implicit a: Monad[M], am: Gen[M[Int]],
                af: Gen[Int => M[Int]], ag: Gen[M[Int => Int]], e: Equal[M[Int]]): Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.monadAll, monad.laws[M], bind.all[M], applicative.all[M])
}
