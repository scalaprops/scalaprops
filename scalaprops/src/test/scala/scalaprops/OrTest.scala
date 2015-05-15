package scalaprops

import scalaz._
import scalaz.std.anyVal._

object OrTest extends Scalaprops {

  val law = scalazlaws.order.all[
    Maybe[Byte] :-: Boolean :-: IList[Int] :-: Or.Empty
  ]

}
