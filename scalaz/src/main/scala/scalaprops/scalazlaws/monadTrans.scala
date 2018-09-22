package scalaprops
package scalazlaws

import scalaz._
import Property.forAll
import ScalapropsScalaz._

object monadTrans {

  def law1[F[_[_], _], G[_], A](implicit
    M: Monad[({type l[a] = F[G, a]})#l],
    F: MonadTrans[F],
    G: Monad[G],
    A: Gen[A],
    E: Equal[F[G, A]]
  ): Property = forAll{ a: A =>
    E.equal(F.liftM(Monad[G].point(a)), M.point(a))
  }

  def law2[F[_[_], _], G[_], A, B](implicit
    M: Monad[({type l[a] = F[G, a]})#l],
    F: MonadTrans[F],
    G: Monad[G],
    GA: Gen[G[A]],
    AGB: Gen[A => G[B]],
    E: Equal[F[G, B]]
  ): Property = forAll{ (ga: G[A], f: A => G[B]) =>
    E.equal(
      F.liftM(G.bind(ga)(f)),
      M.bind(F.liftM(ga))(f.andThen(F.liftM(_)))
    )
  }

  def all[F[_[_], _]: MonadTrans](implicit
    I: Monad[({type l[a] = F[IList, a]})#l],
    M: Monad[({type l[a] = F[Maybe, a]})#l],
    E1: Equal[F[Maybe, Int]],
    E2: Equal[F[IList, Int]]
  ): Properties[ScalazLaw] = Properties.properties(ScalazLaw.monadTrans)(
    ScalazLaw.monadTransLaw1Maybe -> law1[F, Maybe, Int],
    ScalazLaw.monadTransLaw1IList -> law1[F, IList, Int],
    ScalazLaw.monadTransLaw2Maybe -> law2[F, Maybe, Int, Int],
    ScalazLaw.monadTransLaw2IList -> law2[F, IList, Int, Int]
  )

}
