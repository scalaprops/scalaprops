package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.option._

object OptionTTest extends Scalaprops{
  val testIList =
    scalazlaws.monadPlus.all[({type l[a] = OptionT[IList, a]})#l]
  val testMaybe =
    scalazlaws.monadPlus.all[({type l[a] = OptionT[Maybe, a]})#l]
}
