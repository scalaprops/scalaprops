package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._

object TreeTest extends Scalaprops {

  val laws = Properties.list(
    scalazlaws.traverse1.all[Tree],
    scalazlaws.align.all[Tree],
    scalazlaws.zip.all[Tree],
    scalazlaws.comonad.all[Tree],
    scalazlaws.monad.all[Tree]
  )

  val order = scalazlaws.order.all[Tree[Int]]

  val treeGenSized = Property.forAllG(Gen.positiveByte, Gen[Long]){ (n, seed) =>
    val size = 5
    val a = Gen.treeGenSized[Unit](n).samples(
      listSize = size, seed = seed
    ).map(Foldable[Tree].length)

    a == List.fill(size)(n)
  }

  val treeGenSize = {
    val F = Foldable[Tree]
    val p = { (size: Int) =>
      Property.forAll{ tree: Tree[Int] =>
        val c = F.count(tree)
        (c <= size) && ((c * 0.7) < F.toIList(tree).distinct.length)
      }.toProperties(size.toString).andThenParam(
        Param.maxSize(size)
      )
    }

    Properties.fromProps(
      "Gen[Tree]",
      p(1000)
    )
  }

}
