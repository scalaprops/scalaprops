package scalaprops

import scalaz._
import scalaz.std.anyVal._

object ConstTest extends Scalaprops {

  val testInt =
    Properties.list(
      scalazlaws.applicative.all[({type l[a] = Const[Int, a]})#l],
      scalazlaws.order.all[Const[Int, Int]]
    )

}
