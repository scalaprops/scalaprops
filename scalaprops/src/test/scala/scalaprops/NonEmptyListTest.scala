package scalaprops

import scalaz._
import scalaz.std.anyVal._

object NonEmptyListTest extends Scalaprops {

  val testLaws = Properties.list(
    scalazlaws.traverse1.all[NonEmptyList],
    scalazlaws.monad.all[NonEmptyList],
    scalazlaws.comonad.all[NonEmptyList],
    scalazlaws.plus.all[NonEmptyList]
  )
}
