package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._

object ConstTest extends Scalaprops {

  val testInt = {
    Properties.either(
      "Const laws",
      scalazlaws.applicative.all[({type l[a] = Const[Int, a]})#l],
      scalazlaws.order.all[Const[Int, Int]]
    )
  }

}
