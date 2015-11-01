package scalaprops

import scalaz._
import scalaz.std.anyVal._

object ZipperTest extends Scalaprops {

  val testLaw =
    Properties.list(
      scalazlaws.comonad.all[Zipper],
      scalazlaws.apply.all[Zipper],
      scalazlaws.traverse1.all[Zipper],
      scalazlaws.equal.all[Zipper[Int]]
    )

  val applicative = {
    implicit def zipperEqual[A](implicit A: Equal[A]): Equal[Zipper[A]] = {
      import scalaz.std.stream._
      val n = 1000
      Equal.equal{ (x, y) =>
        A.equal(x.focus, y.focus) &&
        Equal[Stream[A]].equal(x.lefts.take(n), y.lefts.take(n)) &&
        Equal[Stream[A]].equal(x.rights.take(n), y.rights.take(n))
      }
    }
    scalazlaws.applicative.laws[Zipper]
  }

}
