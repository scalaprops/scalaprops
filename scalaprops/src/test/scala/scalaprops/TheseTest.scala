package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.tuple._
import scalaz.std.string._

object TheseTest extends Scalaprops {

  private type F[A] = Int \&/ A

  val test0 =
    Properties.either(
      "These",
      scalazlaws.equal.all[Int \&/ Int],
      scalazlaws.semigroup.all[Int \&/ Int]
    )

  val test1 =
    Properties.either(
      "These",
      scalazlaws.monad.all[F],
      scalazlaws.cobind.all[F],
      scalazlaws.traverse.all[F]
    )

  val test2 =
    Properties.either(
      "These",
      scalazlaws.bitraverse.all[\&/]
    )
}
