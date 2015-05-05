package scalaprops

import scalaz._
import scalaz.std.string._
import scalaz.std.anyVal._
import scalaz.std.tuple._

object CokleisliTest extends Scalaprops {

  override val param: Param = Param.withCurrentTimeSeed().copy(maxSize = 30)

  import FunctionEqual._

  private implicit def cokleisliEqual[F[_], A, B](implicit F: Equal[F[A] => B]): Equal[Cokleisli[F, A, B]] =
    F.contramap(_.run)

  val testLaws = {
    type C1[A] = Cokleisli[Maybe, Int, A]
    type C2[A, B] = Cokleisli[NonEmptyList, A, B]

    Properties.either(
      "Cokleisli laws",
      scalazlaws.monad.all[C1],
      scalazlaws.arrow.all[C2]
    )
  }

}
