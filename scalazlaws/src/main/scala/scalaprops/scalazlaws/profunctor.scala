package scalaprops
package scalazlaws

import scalaz._
import Property.forAll

object profunctor {

  def identity[F[_, _], A1, A2](implicit F: Profunctor[F], G: Gen[F[A1, A2]], E: Equal[F[A1, A2]]): Property =
    forAll(F.profunctorLaw.identity[A1, A2] _)

  def composite[F[_, _], A1: Gen, A2: Gen: Cogen, A3: Cogen, A4: Cogen, A5: Gen: Cogen, A6: Gen](implicit F: Profunctor[F], G: Gen[F[A1, A4]], E: Equal[F[A3, A6]]): Property =
    forAll(F.profunctorLaw.composite[A1, A2, A3, A4, A5, A6] _)

  def laws[F[_, _]](implicit F: Profunctor[F], A1: Gen[F[Int, Int]], A2: Equal[F[Int, Int]]): Properties[ScalazLaw] = {
    implicit val a = F.covariantInstance[Int]
    implicit val b = F.contravariantInstance[Int]

    Properties.fromProps(
      ScalazLaw.profunctor,
      Properties.single(ScalazLaw.profunctorIdentity, identity[F, Int, Int]),
      Properties.single(ScalazLaw.profunctorComposite, composite[F, Int, Int, Int, Int, Int, Int]),
      scalazlaws.functor.all[({type l[a] = F[Int, a]})#l],
      scalazlaws.contravariant.all[({type l[a] = F[a, Int]})#l]
    )
  }

  def all[F[_, _]](implicit F: Profunctor[F], A1: Gen[F[Int, Int]], A2: Equal[F[Int, Int]]): Properties[ScalazLaw] =
    laws[F]
}
