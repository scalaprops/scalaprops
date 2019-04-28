package scalaprops

import scalaz._
import scalaz.std.anyVal._

object ChooseTest extends Scalaprops {

  private[this] def test[A: Choose: Gen: Order: Enum](listSize: Int = 10000) = Properties.list(
    Property.forAll{ (from: A, to: A, seed: Long) =>
      val values = Choose[A].choose(from, to).samples(seed = seed, listSize = listSize)
      import scalaz.syntax.order._
      implicit val o = Order[A].toScalaOrdering
      assert(values.max <= Order[A].max(from, to))
      assert(values.min >= Order[A].min(from, to))
      true
    }.toProperties("choose")
    ,
    Property.forAll{ (from: A, to: A, seed: Long) =>
      val values = ISet.fromList(
        Choose[A].withBoundaries(from, to).samples(seed = seed, listSize = listSize)
      )
      val min = Order[A].min(from, to)
      val max = Order[A].max(from, to)
      assert(Foldable[ISet].maximum(values) == Some(max))
      assert(Foldable[ISet].minimum(values) == Some(min))
      if (from != to) {
        assert(values member Enum[A].succ(min))
        assert(values member Enum[A].pred(max))
      }
      true
    }.toProperties("withBoundaries")
  ).andThenParam(Param.minSuccessful(10))

  val long = test[Long](100000).andThenParam(Param.minSuccessful(3))
  val int = test[Int]()
  val short = test[Short]()
  val byte = test[Byte]()

}
