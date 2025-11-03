package scalaprops
package scalazlaws

import Property.forAll
import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object comonadTrans {
  def law1[F[_[_], _], G[_], A](implicit
    C: Comonad[({ type l[a] = F[G, a] })#l],
    F: ComonadTrans[F],
    G: Comonad[G],
    A: Gen[F[G, A]],
    E: Equal[A]
  ): Property = forAll { (a: F[G, A]) => E.equal(G.copoint(F.lower(a)), C.copoint(a)) }

  def law2[F[_[_], _], G[_], A, B](implicit
    C: Cobind[({ type l[a] = F[G, a] })#l],
    F: ComonadTrans[F],
    G: Cobind[G],
    FGA: Gen[F[G, A]],
    GAB: Gen[G[A] => B],
    E: Equal[G[B]]
  ): Property =
    forAll { (w: F[G, A], f: G[A] => B) =>
      E.equal(
        F.lower(C.extend(w)(f compose F.lower[G, A])),
        G.extend(F.lower(w))(f)
      )
    }

  def all[F[_[_], _]: ComonadTrans](implicit
    C1: Comonad[({ type l[a] = F[NonEmptyList, a] })#l],
    G1: Gen[F[NonEmptyList, Int]]
  ): Properties[ScalazLaw] =
    Properties.properties(ScalazLaw.comonadTrans)(
      ScalazLaw.comonadTransLaw1Nel -> law1[F, NonEmptyList, Int],
      ScalazLaw.comonadTransLaw2Nel -> law2[F, NonEmptyList, Int, Int]
    )
}
