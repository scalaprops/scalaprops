package scalaprops

import scalaz._
import scalaz.std.anyVal._

object EitherTTest extends Scalaprops {

  val iListBindRec =
    scalazlaws.bindRec.laws[({type l[a] = EitherT[IList, Byte, a]})#l].andThenParam(Param.maxSize(1))

  val maybe = {
    type F[A] = EitherT[Maybe, Int, A]
    Properties.list(
      scalazlaws.equal.all[EitherT[Maybe, Int, Int]],
      scalazlaws.monadPlus.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.monadError.all[F, Int],
      scalazlaws.traverse.all[F]
    )
  }

  val maybe2 =
    scalazlaws.bitraverse.all[({type l[a, b] = EitherT[Maybe, a, b]})#l]

  val iList = {
    type F[A] = EitherT[IList, Int, A]
    Properties.list(
      scalazlaws.equal.all[EitherT[IList, Int, Int]],
      scalazlaws.monadPlus.all[F],
      scalazlaws.monadError.all[F, Int],
      scalazlaws.traverse.all[F]
    )
  }

  val nel = {
    type F[A] = EitherT[NonEmptyList, Int, A]
    Properties.list(
      scalazlaws.equal.all[EitherT[NonEmptyList, Int, Int]],
      scalazlaws.monadPlus.all[F],
      scalazlaws.monadError.all[F, Int],
      scalazlaws.traverse.all[F]
    )
  }

  val monadTrans = scalazlaws.monadTrans.all[({type l[f[_], a] = EitherT[f, Int, a]})#l]

  import FunctionEqual._
  private type EitherTByte[F[_], A] = EitherT[F, Byte, A]
  val hoist1 = scalazlaws.hoist.law1[EitherTByte, Maybe, Maybe, Maybe]
  val hoist2 = scalazlaws.hoist.law1[EitherTByte, IList, IList, IList]
  val hoist3 = scalazlaws.hoist.law2[EitherTByte, Maybe]
  val hoist4 = scalazlaws.hoist.law2[EitherTByte, IList]
}
