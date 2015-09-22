package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.stream._

object TreeLocTest extends Scalaprops{

  val laws = Properties.list(
    scalazlaws.equal.all[TreeLoc[Byte]],
    scalazlaws.comonad.all[TreeLoc]
  )

  val applyTest = {
    implicit val treeLocApply: Apply[TreeLoc] = new Apply[TreeLoc] {
      override def map[A, B](fa: TreeLoc[A])(f: A => B) =
        fa map f

      private[this] val ForestInstance: Applicative[TreeLoc.TreeForest] =
        Applicative[Stream].compose[Tree]

      override def ap[A, B](fa0: => TreeLoc[A])(f0: => TreeLoc[A => B]) = {
        lazy val f = f0
        lazy val fa = fa0

        TreeLoc(
          tree = Apply[Tree].ap(fa.tree)(f.tree),
          lefts = ForestInstance.ap(fa.lefts)(f.lefts),
          rights = ForestInstance.ap(fa.rights)(f.rights),
          parents = Apply[Stream].apply2(fa.parents, f.parents){
            (x, y) =>
              (ForestInstance.ap(x._1)(y._1), y._2(x._2), ForestInstance.ap(x._3)(y._3))
          }
        )
      }
    }
    scalazlaws.apply.all[TreeLoc].andThenParam(
      Param.maxSize(50)
    )
  }

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
