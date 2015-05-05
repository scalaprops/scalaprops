package scalaprops
package scalazlaws

import scalaz._
import scalaz.std.tuple._

object bitraverse {

  def laws[F[_, _]](implicit
    fa: Gen[F[Int,Int]],
    F: Bitraverse[F],
    EF: Equal[F[Int, Int]],
    G1: Gen[F[Int, Maybe[Int]]],
    G2: Gen[F[Maybe[Int], Int]]
  ) = Properties.fromProps(
    ScalazLaw.bitraverse -> Maybe.empty[*^*->*],
    traverse.all[({type l[a] = F[a, Int]})#l](F.leftTraverse[Int], implicitly, implicitly, implicitly).mapId((_, Maybe.just(*^*->*.left))),
    traverse.all[({type l[a] = F[Int, a]})#l](F.rightTraverse[Int], implicitly, implicitly, implicitly).mapId((_, Maybe.just(*^*->*.right))),
    bifoldable.all[F],
    bifunctor.laws[F]
  )

  def all[F[_, _]](implicit
    fa: Gen[F[Int,Int]],
    F: Bitraverse[F],
    EF: Equal[F[Int, Int]],
    G1: Gen[F[Int, Maybe[Int]]],
    G2: Gen[F[Maybe[Int], Int]]
  ) = laws[F]

}
