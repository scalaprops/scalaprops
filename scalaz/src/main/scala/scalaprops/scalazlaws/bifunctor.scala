package scalaprops
package scalazlaws

import scalaz.*

object bifunctor {
  def laws[F[_, _]](implicit
    F: Bifunctor[F],
    E: Equal[F[Int, Int]],
    af: Gen[F[Int, Int]],
    axy: Gen[Int => Int]
  ): Properties[(ScalazLaw, *^*->*.T)] =
    Properties.fromProps[(ScalazLaw, *^*->*.T)](
      ScalazLaw.bifunctor -> *^*->*.Empty,
      functor
        .all[({ type l[a] = F[a, Int] })#l](F.leftFunctor[Int], implicitly, implicitly, implicitly)
        .mapId((_, *^*->*.L)),
      functor
        .all[({ type l[a] = F[Int, a] })#l](F.rightFunctor[Int], implicitly, implicitly, implicitly)
        .mapId((_, *^*->*.R))
    )

  def all[F[_, _]](implicit F: Bifunctor[F], E: Equal[F[Int, Int]], af: Gen[F[Int, Int]], axy: Gen[Int => Int]) =
    laws[F]
}
