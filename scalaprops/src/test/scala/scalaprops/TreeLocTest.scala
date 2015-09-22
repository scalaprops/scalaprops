package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.stream._

object TreeLocTest extends Scalaprops{

  // TODO remove https://github.com/scalaz/scalaz/commit/f771380867807d4
  private[this] implicit def treeLocOrder[A: Order]: Order[TreeLoc[A]] =
    new Order[TreeLoc[A]] {
      import scalaz.std.stream._
      import scalaz.std.tuple._

      override def equal(x: TreeLoc[A], y: TreeLoc[A]) =
        TreeLoc.treeLocEqual[A].equal(x, y)

      override def order(x: TreeLoc[A], y: TreeLoc[A]) =
        Divide[Order].deriving4(Function.unlift(TreeLoc.unapply[A])).order(x, y)
    }

  val laws = Properties.list(
    scalazlaws.order.all[TreeLoc[Byte]],
    scalazlaws.comonad.all[TreeLoc]
  )

  val treeLocGenSized = {
    val F: Foldable[TreeLoc.TreeForest] = Foldable[Stream].compose[Tree]

    import syntax.monoid._

    implicit val parentInstance: Foldable[TreeLoc.Parent] =
      new Foldable[TreeLoc.Parent] with Foldable.FromFoldMap[TreeLoc.Parent] {
        def foldMap[A, B: Monoid](fa: TreeLoc.Parent[A])(f: A => B) =
          F.foldMap(fa._1)(f) |+| f(fa._2) |+| F.foldMap(fa._3)(f)
      }

    implicit val treeLocFoldable: Foldable[TreeLoc] =
      new Foldable[TreeLoc] with Foldable.FromFoldMap[TreeLoc] {
        def foldMap[A, B: Monoid](fa: TreeLoc[A])(f: A => B) = List(
          Foldable[Tree].foldMap(fa.tree)(f),
          F.foldMap(fa.lefts)(f),
          F.foldMap(fa.rights)(f),
          Foldable[Stream].compose[TreeLoc.Parent].foldMap(fa.parents)(f)
        ).reduceLeft(_ |+| _)
      }

    Property.forAllG(Gen.positiveByte, Gen[Long]){ (n, seed) =>
      val size = 5
      val a = Gen.treeLocGenSized[Unit](n).samples(
        listSize = size, seed = seed
      ).map(Foldable[TreeLoc].length)

      a == List.fill(size)(n)
    }
  }
}
