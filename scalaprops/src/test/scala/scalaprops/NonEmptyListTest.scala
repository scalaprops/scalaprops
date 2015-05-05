package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._

object NonEmptyListTest extends Scalaprops {

  val testLaws = Properties.either(
    "NonEmptyList laws",
    scalazlaws.traverse1.all[NonEmptyList],
    scalazlaws.monad.all[NonEmptyList],
    scalazlaws.comonad.all[NonEmptyList],
    scalazlaws.plus.all[NonEmptyList]
  )
}
