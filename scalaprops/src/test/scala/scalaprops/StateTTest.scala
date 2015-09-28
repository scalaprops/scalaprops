package scalaprops

import scalaz._
import scalaz.std.tuple._
import scalaz.std.anyVal._
import FunctionEqual._

object StateTTest extends Scalaprops {

  implicit def stateTEqual[F[_]: Monad, A, B](implicit F: Equal[A => F[(A, B)]]): Equal[StateT[F, A, B]] =
    F.contramap(_.apply _)

  val id = {
    type F[A] = State[Byte, A]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.bindRec.laws[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  val bindRecIList = {
    scalazlaws.bindRec.laws[({type l[a] = StateT[IList, Byte, a]})#l]
  }.andThenParam(Param.maxSize(1))

  val testIList = {
    type G[A] = IList[A]
    type F[A] = StateT[G, Int, A]

    Properties.list(
      scalazlaws.monadState.all[({type l[a, b] = StateT[G, a, b]})#l, Byte],
      scalazlaws.monadPlusStrong.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  val testMaybe = {
    type G[A] = Maybe[A]
    type F[A] = StateT[G, Int, A]

    Properties.list(
      scalazlaws.monadState.all[({type l[a, b] = StateT[G, a, b]})#l, Byte],
      scalazlaws.monadPlusStrong.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  val tree = {
    type F[A] = StateT[Tree, Int, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }.andThenParam(Param.minSuccessful(20)).composeParam(Param.maxSize(20))

  val bifunctor = scalazlaws.bifunctor.laws[({type l[a, b] = IndexedStateT[Maybe, Int, a, b]})#l]

  val monadTrans = scalazlaws.monadTrans.all[({type l[f[_], a] = StateT[f, Int, a]})#l]

}
