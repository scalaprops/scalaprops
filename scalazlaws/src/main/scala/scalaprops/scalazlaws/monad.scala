package scalaprops
package scalazlaws

import scalaprops.Property.{forAll, forAllGS}
import scalaprops.Properties.properties
import scalaz._

sealed abstract class monad[F[_, _], G[_], A](implicit
  A1: bind[F, G, A], A2: applicative[F, G, A], A3: G[A]
) {
  def rightIdentity[M[_], X](implicit M: Monad[M], e: Equal[M[X]], a: G[M[X]]): Property

  def leftIdentity[M[_], X, Y](implicit M: Monad[M], emy: Equal[M[Y]], ax: G[X], af: G[F[X, M[Y]]]): Property

  final def laws[M[_]: Monad](implicit am: G[M[A]],
                 af: G[F[A, M[A]]], ag: G[M[F[A, A]]], e: Equal[M[A]]) =
    properties(ScalazLaw.monad)(
      ScalazLaw.monadRightIdentity -> this.rightIdentity[M, A],
      ScalazLaw.monadLeftIdentity -> this.leftIdentity[M, A, A]
    )

  def all[M[_]](implicit a: Monad[M], am: G[M[A]],
                af: G[F[A, M[A]]], ag: G[M[F[A, A]]], e: Equal[M[A]]): Properties[ScalazLaw] =
    Properties.fromProps(ScalazLaw.monadAll, this.laws[M], bind[F, G, A].all[M], applicative[F, G, A].all[M])
}

object monadS extends monad[Fun, GS, Int] {
  def rightIdentity[M[_], X](implicit M: Monad[M], e: Equal[M[X]], a: GS[M[X]]): Property =
    forAllGS(M.monadLaw.rightIdentity[X] _)

  def leftIdentity[M[_], X, Y](implicit M: Monad[M], emy: Equal[M[Y]], ax: GS[X], af: GS[Fun[X, M[Y]]]): Property =
    forAllGS{ (a: X, f: Fun[X, M[Y]]) =>
      M.monadLaw.leftIdentity(a, f.fun)
    }
}

object monad extends monad[Function1, Gen, Int] {
  def rightIdentity[M[_], X](implicit M: Monad[M], e: Equal[M[X]], a: Gen[M[X]]) =
    forAll(M.monadLaw.rightIdentity[X] _)

  def leftIdentity[M[_], X, Y](implicit am: Monad[M], emy: Equal[M[Y]], ax: Gen[X], af: Gen[X => M[Y]]) =
    forAll(am.monadLaw.leftIdentity[X, Y] _)
}
