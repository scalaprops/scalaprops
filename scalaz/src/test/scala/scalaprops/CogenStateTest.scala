package scalaprops

import scalaz.Equal
import scalaz.std.anyVal._
import GenTest._
import ScalapropsScalaz._

@scalajs.js.annotation.JSExportAll
object CogenStateTest extends Scalaprops {
  implicit def cogenStateEqual[A: Equal]: Equal[CogenState[A]] =
    Equal.equal { (x, y) => Equal[Rand].equal(x.rand, y.rand) && Equal[Gen[A]].equal(x.gen, y.gen) }

  implicit def cogenStateGen[A](implicit A: Gen[Gen[A]]): Gen[CogenState[A]] =
    Gen.from2(CogenState.apply[A])

  val laws = scalazlaws.functor.all[CogenState]
  val equal = scalazlaws.equal.all[CogenState[Int]]
}
