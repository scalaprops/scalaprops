package scalaprops

import scalaz._
import scalaz.std.string._
import scalaz.std.tuple._
import scalaz.std.anyVal._

object StateTTest extends Scalaprops {
  import FunctionEqual._

  implicit def stateTEqual[F[_], A, B](implicit F: Equal[A => F[(A, B)]]): Equal[StateT[F, A, B]] =
    F.contramap(_.apply _)

  val testIList = {
    type F[A] = StateT[IList, Int, A]

    Properties.either(
      "StateT[IList, Int, _]",
      scalazlaws.monad.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  val testMaybe = {
    type F[A] = StateT[Maybe, Int, A]

    Properties.either(
      "StateT[Maybe, Int, _]",
      scalazlaws.monad.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }
}
