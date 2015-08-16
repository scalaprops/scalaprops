package scalaprops
package scalazlaws

import scalaprops.Property.{forAll, forAllGS}
import scalaprops.Properties.properties
import scalaz._

sealed abstract class applicative[F[_, _], G[_], A](implicit A1: apply[F, G, A], A2: G[A], A3: G[F[A, A]]) {
  def identity[M[_], X](implicit f: Applicative[M], afx: G[M[X]], ef: Equal[M[X]]): Property

  def homomorphism[M[_], X, Y](implicit ap: Applicative[M], ax: G[X], af: G[F[X, Y]], e: Equal[M[Y]]): Property

  def interchange[M[_], X, Y](implicit ap: Applicative[M], ax: G[X], afx: G[M[F[X, Y]]], e: Equal[M[Y]]): Property

  def mapApConsistency[M[_], X, Y](implicit ap: Applicative[M], ax: G[M[X]], afx: G[F[X, Y]], e: Equal[M[Y]]): Property

  final def laws[M[_]](implicit M: Applicative[M],
                       af: G[M[A]], aff: G[M[F[A, A]]],
                       e: Equal[M[A]]) = properties(ScalazLaw.applicative)(
    ScalazLaw.applicativeIdentity -> this.identity[M, A],
    ScalazLaw.applicativeHomomorphism -> this.homomorphism[M, A, A],
    ScalazLaw.applicativeInterchange -> this.interchange[M, A, A],
    ScalazLaw.applicativeMapConsistentWithAp -> this.mapApConsistency[M, A, A]
  )

  final def all[M[_]](implicit M: Applicative[M], af: G[M[A]],
                aff: G[M[F[A, A]]], e: Equal[M[A]]): Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.applicativeAll, this.laws[M], apply[F, G, A].all[M])
}

object applicative extends applicative[Function1, Gen, Int] {
  def apply[F[_, _], G[_], A](implicit A: applicative[F, G, A]): applicative[F, G, A] = A

  implicit val int: applicative[Function1, Gen, Int] = this

  implicit val sInt: applicative[Fun, GS, Int] =
    new applicative[Fun, GS, Int] {
      def identity[M[_], X](implicit f: Applicative[M], afx: GS[M[X]], ef: Equal[M[X]]): Property =
        forAllGS(f.applicativeLaw.identityAp[X] _)

      def homomorphism[M[_], X, Y](implicit ap: Applicative[M], ax: GS[X], af: GS[Fun[X, Y]], e: Equal[M[Y]]): Property =
        forAllGS { (f: Fun[X, Y], a: X) =>
          ap.applicativeLaw.homomorphism(f.fun, a)
        }

      def interchange[M[_], X, Y](implicit ap: Applicative[M], ax: GS[X], afx: GS[M[Fun[X, Y]]], e: Equal[M[Y]]): Property =
        forAllGS { (f: M[Fun[X, Y]], a: X) =>
          ap.applicativeLaw.interchange(ap.map(f)(_.fun), a)
        }

      def mapApConsistency[M[_], X, Y](implicit ap: Applicative[M], ax: GS[M[X]], afx: GS[Fun[X, Y]], e: Equal[M[Y]]): Property =
        forAllGS { (f: Fun[X, Y], a: M[X]) =>
          ap.applicativeLaw.mapLikeDerived(f.fun, a)
        }
    }


  def identity[F[_], X](implicit f: Applicative[F], afx: Gen[F[X]], ef: Equal[F[X]]) =
    forAll(f.applicativeLaw.identityAp[X] _)

  def homomorphism[F[_], X, Y](implicit ap: Applicative[F], ax: Gen[X], af: Gen[X => Y], e: Equal[F[Y]]) =
    forAll(ap.applicativeLaw.homomorphism[X, Y] _)

  def interchange[F[_], X, Y](implicit ap: Applicative[F], ax: Gen[X], afx: Gen[F[X => Y]], e: Equal[F[Y]]) =
    forAll(ap.applicativeLaw.interchange[X, Y] _)

  def mapApConsistency[F[_], X, Y](implicit ap: Applicative[F], ax: Gen[F[X]], afx: Gen[X => Y], e: Equal[F[Y]]) =
    forAll(ap.applicativeLaw.mapLikeDerived[X, Y] _)
}
