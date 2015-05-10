package scalaprops

import scalaz._
import scalaz.std.string._
import scalaz.std.anyVal._
import scalaz.std.tuple._

object CokleisliTest extends Scalaprops {

  override val param: Param = Param.withCurrentTimeSeed().copy(maxSize = 30)

  private[this] val e = new FunctionEqual(5)

  implicit def cokleisliEqual[F[_], A, B: Equal](implicit F: Gen[F[A]]): Equal[Cokleisli[F, A, B]] = {
    import e._
    Equal[F[A] => B].contramap(_.run)
  }

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
