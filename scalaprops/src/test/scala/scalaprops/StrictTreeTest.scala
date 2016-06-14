package scalaprops

import scalaz._
import scalaz.std.AllInstances._

object StrictTreeTest extends Scalaprops {

  val laws = Properties.list(
    scalazlaws.traverse1.all[StrictTree],
    scalazlaws.align.all[StrictTree],
    scalazlaws.zip.all[StrictTree],
    scalazlaws.comonad.all[StrictTree],
    scalazlaws.monad.all[StrictTree]
  )

  val order = scalazlaws.order.all[StrictTree[Int]]

  val strictTreeGenSized = Property.forAllG(Gen.positiveByte, Gen[Long]){ (n, seed) =>
    val size = 5
    val a = Gen.strictTreeGenSized[Unit](n).samples(
      listSize = size, seed = seed
    ).map(Foldable[StrictTree].length)

    a == List.fill(size)(n)
  }.toProperties((), Param.minSuccessful(10))

  val strictTreeGenSize = {
    val F = Foldable[StrictTree]
    val p = { (size: Int) =>
      Property.forAll{ tree: StrictTree[Int] =>
        val c = F.count(tree)
        (c <= size) && ((c * 0.7) < F.toIList(tree).distinct.length)
      }.toProperties(size.toString).andThenParam(
        Param.maxSize(size)
      )
    }

    Properties.fromProps(
      "Gen[StrictTree]",
      p(1000)
    )
  }.andThenParam(Param.minSuccessful(10))

}
