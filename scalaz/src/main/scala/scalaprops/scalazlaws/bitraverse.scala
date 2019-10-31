package scalaprops
package scalazlaws

import scalaz._

object bitraverse {
  def laws[F[_, _]](
    implicit
    fa: Gen[F[Int, Int]],
    F: Bitraverse[F],
    EF: Equal[F[Int, Int]],
    G1: Gen[F[Int, Maybe[Int]]],
    G2: Gen[F[Maybe[Int], Int]]
  ): Properties[(ScalazLaw, *^*->*.T)] = Properties.fromProps(
    ScalazLaw.bitraverse -> *^*->*.Empty,
    traverse
      .all[({ type l[a] = F[a, Int] })#l](F.leftTraverse[Int], implicitly, implicitly, implicitly)
      .mapId((_, *^*->*.L)),
    traverse
      .all[({ type l[a] = F[Int, a] })#l](F.rightTraverse[Int], implicitly, implicitly, implicitly)
      .mapId((_, *^*->*.R)),
    bifoldable.all[F],
    bifunctor.laws[F]
  )

  def all[F[_, _]](
    implicit
    fa: Gen[F[Int, Int]],
    F: Bitraverse[F],
    EF: Equal[F[Int, Int]],
    G1: Gen[F[Int, Maybe[Int]]],
    G2: Gen[F[Maybe[Int], Int]]
  ) = laws[F]
}
