package scalaprops
package scalazlaws

import scalaz._

object profunctor {

  def laws[F[_, _]](implicit F: Profunctor[F], A1: Gen[F[Int, Int]], A2: Equal[F[Int, Int]]): Properties[ScalazLaw] = {
    implicit val a = F.covariantInstance[Int]
    implicit val b = F.contravariantInstance[Int]

    Properties.fromProps(
      ScalazLaw.profunctor,
      scalazlaws.functor.all[({type l[a] = F[Int, a]})#l],
      scalazlaws.contravariant.all[({type l[a] = F[a, Int]})#l]
    )
  }

  def all[F[_, _]](implicit F: Profunctor[F], A1: Gen[F[Int, Int]], A2: Equal[F[Int, Int]]): Properties[ScalazLaw] =
    laws[F]
}
