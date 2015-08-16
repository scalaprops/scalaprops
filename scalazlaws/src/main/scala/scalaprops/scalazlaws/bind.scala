package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz._

sealed abstract class bind[F[_, _], G[_], A](implicit A1: apply[F, G, A]){
  def associativity[M[_], X, Y, Z](implicit M: Bind[M], amx: G[M[X]], af: G[F[X, M[Y]]],
                                   ag: G[F[Y, M[Z]]], emz: Equal[M[Z]]): Property

  def bindApConsistency[M[_], X, Y](implicit M: Bind[M], amx: G[M[X]],
                                    af: G[M[F[X, Y]]], emy: Equal[M[Y]]): Property

  final def laws[M[_]](implicit M: Bind[M], am: G[M[A]],
                 af: G[F[A, M[A]]], ag: G[M[F[A, A]]], e: Equal[M[A]]): Properties[ScalazLaw] =
    Properties.fromChecks(ScalazLaw.bind)(
      ScalazLaw.bindAssociativity -> Check(
        this.associativity[M, A, A, A], Param.maxSize(5)
      ),
      ScalazLaw.bindApConsistentWithBind -> Check(
        this.bindApConsistency[M, A, A], Param.maxSize(10)
      )
    )

  final def all[M[_]](implicit a: Bind[M], am: G[M[A]],
                af: G[F[A, M[A]]], ag: G[M[F[A, A]]], e: Equal[M[A]]): Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.bindAll, this.laws[M], apply[F, G, A].all[M])
}

object bind extends bind[Function1, Gen, Int] {
  def apply[F[_, _], G[_], A](implicit A: bind[F, G, A]): bind[F, G, A] = A

  implicit val int: bind[Function1, Gen, Int] = this

  implicit val sInt: bind[Fun, GS, Int] =
    new bind[Fun, GS, Int] {
      def associativity[M[_], X, Y, Z](implicit M: Bind[M], amx: GS[M[X]], af: GS[Fun[X, M[Y]]],
                                       ag: GS[Fun[Y, M[Z]]], emz: Equal[M[Z]]): Property =
        Property.forAllGS { (fa: M[X], f: Fun[X, M[Y]], g: Fun[Y, M[Z]]) =>
          M.bindLaw.associativeBind[X, Y, Z](fa, f.fun, g.fun)
        }

      def bindApConsistency[M[_], X, Y](implicit M: Bind[M], amx: GS[M[X]],
                                        af: GS[M[Fun[X, Y]]], emy: Equal[M[Y]]): Property =
        Property.forAllGS { (fa: M[X], f: M[Fun[X, Y]]) =>
          M.bindLaw.apLikeDerived[X, Y](fa, M.map(f)(_.fun))
        }
    }

  def associativity[M[_], X, Y, Z](implicit M: Bind[M], amx: Gen[M[X]], af: Gen[(X => M[Y])],
                                   ag: Gen[(Y => M[Z])], emz: Equal[M[Z]]) =
    forAll(M.bindLaw.associativeBind[X, Y, Z] _)

  def bindApConsistency[M[_], X, Y](implicit M: Bind[M], amx: Gen[M[X]],
                                    af: Gen[M[X => Y]], emy: Equal[M[Y]]) =
    forAll(M.bindLaw.apLikeDerived[X, Y] _)
}
