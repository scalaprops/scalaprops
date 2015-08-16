package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz._

trait bind[F[_, _], G[_]] {
   def associativity[M[_], X, Y, Z](implicit M: Bind[M], amx: G[M[X]], af: G[F[X, M[Y]]],
                                   ag: G[F[Y, M[Z]]], emz: Equal[M[Z]]): Property

  def bindApConsistency[M[_], X, Y](implicit M: Bind[M], amx: G[M[X]],
                                    af: G[M[F[X, Y]]], emy: Equal[M[Y]]): Property

  def laws[M[_]](implicit a: Bind[M], am: G[M[Int]],
                 af: G[F[Int, M[Int]]], ag: G[M[F[Int, Int]]], e: Equal[M[Int]]): Properties[ScalazLaw]

  def all[M[_]](implicit a: Bind[M], am: G[M[Int]],
                af: G[F[Int, M[Int]]], ag: G[M[F[Int, Int]]], e: Equal[M[Int]]): Properties[ScalazLaw]
}

object bindS extends bind[Fun, GS] {
  override def associativity[M[_], X, Y, Z](implicit M: Bind[M], amx: GS[M[X]], af: GS[Fun[X, M[Y]]], ag: GS[Fun[Y, M[Z]]], emz: Equal[M[Z]]): Property = {
    import GS._

    Property.forAllS{ (fa: M[X], f: Fun[X, M[Y]], g: Fun[Y, M[Z]]) =>
      M.bindLaw.associativeBind[X, Y, Z](fa, f.fun, g.fun)
    }
  }

  override def laws[M[_]](implicit a: Bind[M], am: GS[M[Int]], af: GS[Fun[Int, M[Int]]], ag: GS[M[Fun[Int, Int]]], e: Equal[M[Int]]): Properties[ScalazLaw] = ???

  override def all[M[_]](implicit a: Bind[M], am: GS[M[Int]], af: GS[Fun[Int, M[Int]]], ag: GS[M[Fun[Int, Int]]], e: Equal[M[Int]]): Properties[ScalazLaw] = ???

  override def bindApConsistency[M[_], X, Y](implicit M: Bind[M], amx: GS[M[X]], af: GS[M[Fun[X, Y]]], emy: Equal[M[Y]]): Property = ???
}

object bind extends bind[Function1, Gen] {
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
