package scalaprops

import scalaz._
import scalaz.std.anyVal._
import ScalapropsScalaz._

object TreeTest extends Scalaprops {
  val laws = Properties.list(
    scalazlaws.traverse1.all[scalaz.Tree],
    scalazlaws.align.all[scalaz.Tree],
    scalazlaws.zip.all[scalaz.Tree],
    scalazlaws.comonad.all[scalaz.Tree],
    scalazlaws.monad.all[scalaz.Tree]
  )

  val order = scalazlaws.order.all[scalaz.Tree[Int]]

  val treeGenSized = Property
    .forAllG(Gen.positiveByte, Gen[Long]) { (n, seed) =>
      val size = 5
      val a = ScalapropsScalaz
        .treeGenSized[Unit](n)
        .samples(
          listSize = size,
          seed = seed
        )
        .map(Foldable[scalaz.Tree].length)

      a == List.fill(size)(n)
    }
    .toProperties((), Param.minSuccessful(10))

  def distinctStream[A: Order](s: Stream[A]): Int \/ Stream[A] = {
    def loop(seen: ISet[A], rest: Stream[A], i: Int): Int \/ Stream[A] = {
      if i > (seen.size * 200) then {
        -\/(seen.size)
      } else {
        rest match {
          case h #:: t =>
            if seen.contains(h) then {
              loop(seen, t, i + 1)
            } else {
              loop(seen insert h, t, 0).map { x => Stream.cons(h, x) }
            }
          case _ =>
            // stream is finite !?
            \/-(rest)
        }
      }
    }
    loop(ISet.empty[A], s, 0)
  }

  def sizeTest[A: Order](numbers: List[Int], f: (Int, Long) => Stream[A]) = {
    val sizes = numbers.zipWithIndex.map(t => t.copy(_2 = t._2 + 1))

    val tests = sizes.map { case (n, i) =>
      Property.forAll { (seed: Long) =>
        val s = f(i, seed)
        distinctStream(s) match {
          case -\/(x) =>
            assert(x == n, s"$x $n")
            true
          case \/-(x) =>
            sys.error(x.size.toString)
        }
      }.toProperties(i.toString, Param.minSuccessful(1))
    }

    Properties.list(tests.head, tests.tail*)
  }

  val treeGenSize = {
    val F = Foldable[scalaz.Tree]
    val p = { (size: Int) =>
      Property.forAll { (tree: scalaz.Tree[Int]) =>
        val c = F.count(tree)
        (c <= size) && ((c * 0.7) < F.toIList(tree).distinct.length)
      }.toProperties(size.toString)
        .andThenParam(
          Param.maxSize(size)
        )
    }

    Properties.fromProps(
      "Gen[Tree]",
      p(1000)
    )
  }.andThenParam(Param.minSuccessful(10))
}
