package scalaprops
package scalazlaws

import scalaprops.Property.{forAll, forAllGS}
import scalaprops.Properties.properties
import scalaz._

sealed abstract class invariantFunctor[F[_, _], G[_], A] extends Serializable {

  def identity[M[_], X](implicit F: InvariantFunctor[M], afx: G[M[X]], ef: Equal[M[X]]): Property

  def composite[M[_], X, Y, Z](implicit M: InvariantFunctor[M], af: G[M[X]], axy: G[F[X, Y]],
                               ayz: G[F[Y, Z]], ayx: G[F[Y, X]], azy: G[F[Z, Y]], ef: Equal[M[Z]]): Property

  final def laws[M[_]](implicit M: InvariantFunctor[M], af: G[M[A]], axy: G[F[A, A]],
                 ef: Equal[M[A]]): Properties[ScalazLaw] =
    properties(ScalazLaw.invariantFunctor) (
      ScalazLaw.invariantFunctorIdentity -> identity[M, A],
      ScalazLaw.invariantFunctorComposite -> composite[M, A, A, A]
    )

  final def all[M[_]](implicit M: InvariantFunctor[M], af: G[M[A]], axy: G[F[A, A]],
                ef: Equal[M[A]]): Properties[ScalazLaw] =
    laws[M]
}


object invariantFunctor extends invariantFunctor[Function1, Gen, Int] {
  def apply[F[_, _], G[_], A](implicit A: invariantFunctor[F, G, A]): invariantFunctor[F, G, A] = A

  implicit val sInt: invariantFunctor[Fun, GS, Int] = new invariantFunctor[Fun, GS, Int] {
    def identity[M[_], X](implicit F: InvariantFunctor[M], afx: GS[M[X]], ef: Equal[M[X]]): Property =
      forAllGS(F.invariantFunctorLaw.invariantIdentity[X] _)

    def composite[M[_], X, Y, Z](implicit M: InvariantFunctor[M], af: GS[M[X]], axy: GS[Fun[X, Y]],
                                 ayz: GS[Fun[Y, Z]], ayx: GS[Fun[Y, X]], azy: GS[Fun[Z, Y]], ef: Equal[M[Z]]): Property =
      forAllGS { (ma: M[X], f1: Fun[X, Y], f2: Fun[Y, X], f3: Fun[Y, Z], f4: Fun[Z, Y]) =>
        M.invariantFunctorLaw.invariantComposite(
          ma, f1.fun, f2.fun, f3.fun, f4.fun
        )
      }
  }

  implicit val int: invariantFunctor[Function1, Gen, Int] = this

  def identity[F[_], X](implicit F: InvariantFunctor[F], afx: Gen[F[X]], ef: Equal[F[X]]): Property =
    forAll(F.invariantFunctorLaw.invariantIdentity[X] _)

  def composite[F[_], X, Y, Z](implicit F: InvariantFunctor[F], af: Gen[F[X]], axy: Gen[(X => Y)],
                               ayz: Gen[Y => Z], ayx: Gen[Y => X], azy: Gen[Z => Y], ef: Equal[F[Z]]): Property =
    forAll(F.invariantFunctorLaw.invariantComposite[X, Y, Z] _)
}
