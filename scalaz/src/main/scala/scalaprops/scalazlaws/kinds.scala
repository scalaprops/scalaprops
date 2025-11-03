package scalaprops
package scalazlaws

import scalaz.*
import scalaz.std.string.*

final class *^*->* private (override val toString: String) {}

object *^*->* {
  type T = *^*->* :-: Unit :-: Or.Empty

  val left = new *^*->*("left")
  val right = new *^*->*("right")

  val L: T = Or[T](left)
  val R: T = Or[T](right)
  val Empty: T = Or[T](())

  implicit val instance: Order[*^*->*] =
    Order.orderBy(_.toString)
}
