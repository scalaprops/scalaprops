package scalaprops
package scalazlaws

import scalaz._
import Property.forAll

object hoist {

  def law1[M[_[_], _], F[_], G[_], H[_]](implicit
    M: Hoist[M],
    F: Monad[F],
    G: Monad[G],
    FG: Gen[F ~> G],
    GH: Gen[G ~> H],
    E: Equal[({type l[a] = M[F, a]})#l ~> ({type l[a] = M[H, a]})#l]
  ) = forAll{ (fg: F ~> G, gh: G ~> H) =>
    E.equal(
      M.hoist(gh.compose(fg)),
      M.hoist(gh).compose[({type l[a] = M[F, a]})#l](M.hoist(fg))
    )
  }

  def law2[M[_[_], _], F[_]](implicit
    M: Hoist[M],
    F: Monad[F],
    E: Equal[({type l[a] = M[F, a]})#l ~> ({type l[a] = M[F, a]})#l]
  ) = forAll{
    E.equal(
      M.hoist(NaturalTransformation.refl[F]),
      NaturalTransformation.refl[({type l[a] = M[F, a]})#l]
    )
  }

}
