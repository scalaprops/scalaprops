package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._

object TreeTest extends Scalaprops {

  val traverse1 = scalazlaws.traverse1.all[Tree]
  val monad = scalazlaws.monad.all[Tree]
  val equal = scalazlaws.equal.all[Tree[Int]]

  // TODO test comonad law

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
