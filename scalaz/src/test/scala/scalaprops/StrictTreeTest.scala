package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.AllInstances.*

object StrictTreeTest extends Scalaprops {
  property("laws") = Properties.list(
    scalazlaws.traverse1.all[StrictTree],
    scalazlaws.align.all[StrictTree],
    scalazlaws.zip.all[StrictTree],
    scalazlaws.comonad.all[StrictTree],
    scalazlaws.monad.all[StrictTree]
  )

  property("order") = scalazlaws.order.all[StrictTree[Int]]

  property("strictTreeGenSized") = Property
    .forAllG(Gen.positiveByte, Gen[Long]) { (n, seed) =>
      val size = 5
      val a = ScalapropsScalaz
        .strictTreeGenSized[Unit](n)
        .samples(
          listSize = size,
          seed = seed
        )
        .map(Foldable[StrictTree].length)

      a == List.fill(size)(n)
    }
    .toProperties((), Param.minSuccessful(10))

  property("strictTreeGenSize") = {
    val F = Foldable[StrictTree]
    val p = { (size: Int) =>
      Property.forAll { (tree: StrictTree[Int]) =>
        val c = F.count(tree)
        (c <= size) && ((c * 0.7) < F.toIList(tree).distinct.length)
      }.toProperties(size.toString)
        .andThenParam(
          Param.maxSize(size)
        )
    }

    Properties.fromProps(
      "Gen[StrictTree]",
      p(1000)
    )
  }.andThenParam(Param.minSuccessful(10))
}
