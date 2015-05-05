package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._
import scalaz.std.tuple._

object EitherTTest extends Scalaprops {

  val testEitherTMaybe =
    Properties.either(
      "EitherT[Maybe, _, _]",
      scalazlaws.monadPlus.all[({type l[a] = EitherT[Maybe, Int, a]})#l],
      scalazlaws.traverse.all[({type l[a] = EitherT[Maybe, Int, a]})#l]
    )

  val testEitherTMaybe2 =
    Properties.either(
      "EitherT[Maybe, _, _]",
      scalazlaws.bitraverse.all[({type l[a, b] = EitherT[Maybe, a, b]})#l]
    )

  val testEitherTIList =
    Properties.either(
      "EitherT[IList, _, _]",
      scalazlaws.monadPlus.all[({type l[a] = EitherT[IList, Int, a]})#l],
      scalazlaws.traverse.all[({type l[a] = EitherT[IList, Int, a]})#l]
    )

  val testEitherTNel =
    Properties.either(
      """EitherT[NonEmptyList, _, _]""",
      scalazlaws.monad.all[({type l[a] = EitherT[NonEmptyList, Int, a]})#l],
      scalazlaws.traverse.all[({type l[a] = EitherT[NonEmptyList, Int, a]})#l]
    )

}
