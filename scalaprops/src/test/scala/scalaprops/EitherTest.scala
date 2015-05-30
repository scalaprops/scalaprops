package scalaprops

import scalaz.std.either._
import scalaz.std.anyVal._

object EitherTest extends Scalaprops {

  val bitraverse = scalazlaws.bitraverse.all[Either]
  val associative = scalazlaws.associative.all[Either]
  val order = scalazlaws.order.all[Int Either Int]

  val laws1 = {
    type T[A] = Either[Int, A]
    Properties.list(
      scalazlaws.monad.all[T],
      scalazlaws.traverse.all[T]
    )
  }

}
