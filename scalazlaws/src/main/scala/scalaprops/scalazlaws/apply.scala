package scalaprops
package scalazlaws

import scalaprops.Property.{forAll, forAllGS}
import scalaz._

sealed abstract class apply[F[_, _], G[_], A](implicit A1: functor[F, G, A], A2: G[F[A, A]]) {
  def composition[M[_], X, Y, Z](implicit ap: Apply[M], afx: G[M[X]], au: G[M[F[Y, Z]]],
                                 av: G[M[F[X, Y]]], e: Equal[M[Z]]): Property

  final def laws[M[_]](implicit M: Apply[M], af: G[M[A]],
                 aff: G[M[F[A, A]]], e: Equal[M[A]]): Properties[ScalazLaw] =
    Properties.fromChecks(ScalazLaw.apply)(
      ScalazLaw.applyComposition -> Check(
        composition[M, A, A, A], Param.maxSize(5)
      )
    )

  final def all[M[_]](implicit M: Apply[M], af: G[M[A]],
                aff: G[M[F[A, A]]], e: Equal[M[A]]): Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.applyAll, this.laws[M], functor[F, G, A].all[M])
}



object apply extends apply[Function1, Gen, Int] {
  def apply[F[_, _], G[_], A](implicit A: apply[F, G, A]): apply[F, G, A] = A

  implicit val int: apply[Function1, Gen, Int] = this
  implicit val sInt: apply[Fun, GS, Int] =
    new apply[Fun, GS, Int] {
      def composition[M[_], X, Y, Z](implicit M: Apply[M], afx: GS[M[X]], au: GS[M[Fun[Y, Z]]],
                                     av: GS[M[Fun[X, Y]]], e: Equal[M[Z]]) =
        forAllGS { (f1: M[Fun[Y, Z]], f2: M[Fun[X, Y]], ma: M[X]) =>
          M.applyLaw.composition(
            M.map(f1)(_.fun), M.map(f2)(_.fun), ma
          )
        }
    }

  def composition[M[_], X, Y, Z](implicit M: Apply[M], afx: Gen[M[X]], au: Gen[M[Y => Z]],
                                 av: Gen[M[X => Y]], e: Equal[M[Z]]) =
    forAll(M.applyLaw.composition[X, Y, Z] _)
}
