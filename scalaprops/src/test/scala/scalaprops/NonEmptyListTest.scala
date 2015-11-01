package scalaprops

import scalaz._
import scalaz.std.anyVal._

object NonEmptyListTest extends Scalaprops {

  val testLaws = Properties.list(
    scalazlaws.traverse1.all[NonEmptyList],
    scalazlaws.monad.all[NonEmptyList],
    scalazlaws.zip.all[NonEmptyList],
    scalazlaws.align.all[NonEmptyList],
    scalazlaws.comonad.all[NonEmptyList],
    scalazlaws.plus.all[NonEmptyList]
  )

  val size = Property.forAllG(Gen.positiveByte, Gen[Long]){ (s, seed) =>
    Gen[NonEmptyList[Unit]].sample(s, seed).size <= s
  }

  val tailrecBindConsistency = scalazlaws.bindRec.tailrecBindConsistency[NonEmptyList, Byte]
  val handleManyBinds = scalazlaws.bindRec.handleManyBinds[NonEmptyList, Byte](1000000).toProperties(
    "handleManyBinds", Param.maxSize(1).andThen(Param.minSuccessful(10))
  )
}
