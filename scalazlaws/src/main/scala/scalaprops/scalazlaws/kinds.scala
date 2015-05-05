package scalaprops
package scalazlaws

import scalaz._
import scalaz.std.string._

final class *^*->* private(override val toString: String) {}

object *^*->* {
  val left = new *^*->*("left")
  val right = new *^*->*("right")

  implicit val instance: Order[*^*->*] =
    Order.orderBy(_.toString)
}
