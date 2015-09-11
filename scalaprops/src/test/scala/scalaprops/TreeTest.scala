package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._

object TreeTest extends Scalaprops {

  val laws = Properties.list(
    scalazlaws.traverse1.all[Tree],
    scalazlaws.align.all[Tree],
    scalazlaws.comonad.all[Tree],
    scalazlaws.monad.all[Tree]
  )

  val equal = scalazlaws.equal.all[Tree[Int]]

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
