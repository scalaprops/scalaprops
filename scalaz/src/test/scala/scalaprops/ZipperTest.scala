package scalaprops

import scalaz._
import scalaz.std.anyVal._
import ScalapropsScalaz._

object ZipperTest extends Scalaprops {
  val size = Property.forAllG(Gen.choose(1, 300), Gen[Rand]) { (size, rand) =>
    Foldable[Zipper].length(Gen[Zipper[Unit]].f(size, rand)._2) == size
  }

  val testLaw =
    Properties.list(
      scalazlaws.comonad.all[Zipper],
      scalazlaws.apply.all[Zipper],
      scalazlaws.traverse1.all[Zipper],
      scalazlaws.equal.all[Zipper[Int]]
    )

  val applicative = {
    implicit def zipperEqual[A](implicit A: Equal[A]): Equal[Zipper[A]] = {
      import scalaz.std.lazylist._
      val n = 1000
      Equal.equal { (x, y) =>
        A.equal(x.focus, y.focus) &&
        Equal[LazyList[A]].equal(x.lefts.take(n), y.lefts.take(n)) &&
        Equal[LazyList[A]].equal(x.rights.take(n), y.rights.take(n))
      }
    }
    scalazlaws.applicative.laws[Zipper]
  }
}
