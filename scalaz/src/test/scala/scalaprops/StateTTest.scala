package scalaprops

import FunctionEqual.*
import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*
import scalaz.std.either.*
import scalaz.std.tuple.*

object StateTTest extends Scalaprops {
  implicit def stateTEqual[F[_]: Monad, A, B](implicit F: Equal[A => F[(A, B)]]): Equal[StateT[A, F, B]] =
    F.contramap(_.apply _)

  property("id") = {
    type F[A] = State[Byte, A]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.bindRec.laws[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  property("bindRecIList") = {
    scalazlaws.bindRec.laws[({ type l[a] = StateT[Byte, IList, a] })#l]
  }.andThenParam(Param.maxSize(1))

  property("iList") = {
    type F[A] = StateT[Int, IList, A]

    Properties.list(
      scalazlaws.monadPlusStrong.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  property("maybe") = {
    type F[A] = StateT[Int, Maybe, A]

    Properties.list(
      scalazlaws.monadPlusStrong.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  property("monadError") = {
    type F[A] = StateT[Int, ({ type l[a] = Either[Int, a] })#l, A]
    scalazlaws.monadError.all[F, Int]
  }

  property("tree") = {
    type F[A] = StateT[Int, Tree, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }.andThenParam(Param.minSuccessful(20)).composeParam(Param.maxSize(20))

  property("bifunctor") = scalazlaws.bifunctor.laws[({ type l[a, b] = IndexedStateT[Int, a, Maybe, b] })#l]

  property("monadTrans") = scalazlaws.monadTrans.all[({ type l[f[_], a] = StateT[Int, f, a] })#l]
}
