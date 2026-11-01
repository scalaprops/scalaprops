package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object ZipperTest extends Scalaprops {
  property("size") = Property.forAllG(Gen.choose(1, 300), Gen[Rand]) { (size, rand) =>
    Foldable[Zipper].length(Gen[Zipper[Unit]].f(size, rand)._2) == size
  }

  property("law") = Properties.list(
    scalazlaws.comonad.all[Zipper],
    scalazlaws.apply.all[Zipper],
    scalazlaws.traverse1.all[Zipper],
    scalazlaws.equal.all[Zipper[Int]]
  )

  property("applicative") = {
    implicit def zipperEqual[A](implicit A: Equal[A]): Equal[Zipper[A]] = {
      import scalaz.std.stream.*
      val n = 1000
      Equal.equal { (x, y) =>
        A.equal(x.focus, y.focus) &&
        Equal[Stream[A]].equal(x.lefts.take(n), y.lefts.take(n)) &&
        Equal[Stream[A]].equal(x.rights.take(n), y.rights.take(n))
      }
    }
    scalazlaws.applicative.laws[Zipper]
  }
}
