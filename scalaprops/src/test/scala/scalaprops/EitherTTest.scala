package scalaprops

import scalaz._
import scalaz.std.anyVal._

object EitherTTest extends Scalaprops {

  val testEitherTMaybe =
    Properties.list(
      scalazlaws.equal.all[EitherT[Maybe, Int, Int]],
      scalazlaws.monadPlus.all[({type l[a] = EitherT[Maybe, Int, a]})#l],
      scalazlaws.monadError.all[({type l[a, b] = EitherT[Maybe, a, b]})#l, Int],
      scalazlaws.traverse.all[({type l[a] = EitherT[Maybe, Int, a]})#l]
    )

  val testEitherTMaybe2 =
    scalazlaws.bitraverse.all[({type l[a, b] = EitherT[Maybe, a, b]})#l]

  val testEitherTIList =
    Properties.list(
      scalazlaws.equal.all[EitherT[IList, Int, Int]],
      scalazlaws.monadPlus.all[({type l[a] = EitherT[IList, Int, a]})#l],
      scalazlaws.monadError.all[({type l[a, b] = EitherT[IList, a, b]})#l, Int],
      scalazlaws.traverse.all[({type l[a] = EitherT[IList, Int, a]})#l]
    )

  val testEitherTNel =
    Properties.list(
      scalazlaws.equal.all[EitherT[NonEmptyList, Int, Int]],
      scalazlaws.monadPlus.all[({type l[a] = EitherT[NonEmptyList, Int, a]})#l],
      scalazlaws.monadError.all[({type l[a, b] = EitherT[NonEmptyList, a, b]})#l, Int],
      scalazlaws.traverse.all[({type l[a] = EitherT[NonEmptyList, Int, a]})#l]
    )

  val monadTrans = scalazlaws.monadTrans.all[({type l[f[_], a] = EitherT[f, Int, a]})#l]

  import FunctionEqual._
  private type EitherTByte[F[_], A] = EitherT[F, Byte, A]
  val hoist1 = scalazlaws.hoist.law1[EitherTByte, Maybe, Maybe, Maybe]
  val hoist2 = scalazlaws.hoist.law1[EitherTByte, IList, IList, IList]
  val hoist3 = scalazlaws.hoist.law2[EitherTByte, Maybe]
  val hoist4 = scalazlaws.hoist.law2[EitherTByte, IList]
}
