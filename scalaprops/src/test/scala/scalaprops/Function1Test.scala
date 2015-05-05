package scalaprops

import scalaz.std.function._
import scalaz.std.anyVal._
import scalaz.std.tuple._
import scalaz.std.string._

object Function1Test extends Scalaprops {

  import FunctionEqual._

  val testLaws1 = {
    type F1[A] = Int => A
    type F2[A] = A => Int

    Properties.either(
      "function1",
      scalazlaws.monad.all[F1],
      scalazlaws.comonad.all[F1],
      scalazlaws.zip.all[F1],
      scalazlaws.contravariant.all[F2],
      scalazlaws.arrow.all[Function1]
    )
  }

  val testLaws2 = scalazlaws.monoid.all[Int => Int]
}
