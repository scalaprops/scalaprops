package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz._

object bind {
  def associativity[M[_], X, Y, Z](implicit M: Bind[M], amx: Gen[M[X]], af: Gen[(X => M[Y])],
                                   ag: Gen[(Y => M[Z])], emz: Equal[M[Z]]) =
    forAll(M.bindLaw.associativeBind[X, Y, Z] _)

  def bindApConsistency[M[_], X, Y](implicit M: Bind[M], amx: Gen[M[X]],
                                    af: Gen[M[X => Y]], emy: Equal[M[Y]]) =
    forAll(M.bindLaw.apLikeDerived[X, Y] _)

  def laws[M[_]](implicit a: Bind[M], am: Gen[M[Int]],
                 af: Gen[Int => M[Int]], ag: Gen[M[Int => Int]], e: Equal[M[Int]]) =
    Properties.fromChecks(ScalazLaw.bind)(
      ScalazLaw.bindAssociativity -> Check(
        bind.associativity[M, Int, Int, Int], Param.maxSize(5)
      ),
      ScalazLaw.bindApConsistentWithBind -> Check(
        bind.bindApConsistency[M, Int, Int], Param.maxSize(10)
      )
    )

  def all[M[_]](implicit a: Bind[M], am: Gen[M[Int]],
                af: Gen[Int => M[Int]], ag: Gen[M[Int => Int]], e: Equal[M[Int]]): Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.bindAll, bind.laws[M], apply.all[M])
}
