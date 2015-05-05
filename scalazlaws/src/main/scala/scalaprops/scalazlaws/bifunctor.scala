package scalaprops
package scalazlaws

import scalaz._
import scalaz.std.tuple._

object bifunctor {

  def laws[F[_, _]](implicit F: Bifunctor[F], E: Equal[F[Int, Int]], af: Gen[F[Int, Int]],
                    axy: Gen[Int => Int]) =
    Properties.fromProps(
      ScalazLaw.bifunctor -> Maybe.empty[*^*->*],
      functor.all[({type l[a] = F[a, Int]})#l](F.leftFunctor[Int], implicitly, implicitly, implicitly).mapId((_, Maybe.just(*^*->*.left))),
      functor.all[({type l[a] = F[Int, a]})#l](F.rightFunctor[Int], implicitly, implicitly, implicitly).mapId((_, Maybe.just(*^*->*.right)))
    )

  def all[F[_, _]](implicit F: Bifunctor[F], E: Equal[F[Int, Int]], af: Gen[F[Int, Int]],
                   axy: Gen[Int => Int]) =
    laws[F]
}
