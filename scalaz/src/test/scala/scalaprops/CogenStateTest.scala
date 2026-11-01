package scalaprops

import GenTest.*
import ScalapropsScalaz.*
import scalaz.Equal
import scalaz.std.anyVal.*

object CogenStateTest extends Scalaprops {
  implicit def cogenStateEqual[A: Equal]: Equal[CogenState[A]] =
    Equal.equal { (x, y) => Equal[Rand].equal(x.rand, y.rand) && Equal[Gen[A]].equal(x.gen, y.gen) }

  implicit def cogenStateGen[A](implicit A: Gen[Gen[A]]): Gen[CogenState[A]] =
    Gen.from2(CogenState.apply[A])

  property("laws") = scalazlaws.functor.all[CogenState]
  property("equal") = scalazlaws.equal.all[CogenState[Int]]
}
