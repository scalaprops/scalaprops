package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.tuple._

object CokleisliTest extends Scalaprops {

  override val param: Param = Param.withCurrentTimeSeed().copy(maxSize = 30)

  private[this] val e = new FunctionEqual(5)

  implicit def cokleisliEqual[F[_], A, B: Equal](implicit F: Gen[F[A]]): Equal[Cokleisli[F, A, B]] = {
    import e._
    Equal[F[A] => B].contramap(_.run)
  }

  val maybe1 = {
    type C[A, B] = Cokleisli[Maybe, A, B]

    Properties.list(
      scalazlaws.compose.all[C],
      scalazlaws.profunctor.all[C]
    )
  }

  val maybe2 = {
    type F[A] = Cokleisli[Maybe, Byte, A]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.bindRec.all[F]
    )
  }

  val nel1 = scalazlaws.arrow.all[({type l[a, b] = Cokleisli[NonEmptyList, a, b]})#l]
  val nel2 = {
    type F[A] = Cokleisli[NonEmptyList, Byte, A]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.bindRec.all[F]
    )
  }

  val tree1 = scalazlaws.arrow.all[({type l[a, b] = Cokleisli[Tree, a, b]})#l]
  val tree2 = {
    type F[A] = Cokleisli[Tree, Byte, A]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.bindRec.all[F]
    )
  }

  val zipper1 = scalazlaws.arrow.all[({type l[a, b] = Cokleisli[Zipper, a, b]})#l]
  val zipper2 = {
    type F[A] = Cokleisli[Zipper, Byte, A]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.bindRec.all[F]
    )
  }

}
