package scalaprops
package internal

import TreeZipper._
import scala.annotation.tailrec

final case class TreeZipper[A](tree: Tree[A], lefts: TreeForest[A], rights: TreeForest[A], parents: Parents[A]) {
  import Tree._

  def parent: Option[TreeZipper[A]] = parents match {
    case (pls, v, prs) #:: ps => Some(TreeZipper(Node(v, combChildren(lefts, tree, rights)), pls, prs, ps))
    case Stream.Empty => None
  }

  @tailrec
  def root: TreeZipper[A] =
    parent match {
      case Some(z) => z.root
      case None => this
    }

  private def left: Option[TreeZipper[A]] = lefts match {
    case t #:: ts => Some(TreeZipper(t, ts, tree #:: rights, parents))
    case Stream.Empty => None
  }

  private def right: Option[TreeZipper[A]] = rights match {
    case t #:: ts => Some(TreeZipper(t, tree #:: lefts, ts, parents))
    case Stream.Empty => None
  }

  private def firstChild: Option[TreeZipper[A]] = tree.subForest match {
    case t #:: ts => Some(TreeZipper(t, Stream.Empty, ts, downParents))
    case Stream.Empty => None
  }

  def toTree: Tree[A] = root.tree

  def getLabel: A = tree.rootLabel

  def map[B](f: A => B): TreeZipper[B] = {
    val ff = (_: Tree[A]).map(f)
    TreeZipper(tree map f, lefts map ff, rights map ff, parents.map {
      case (l, t, r) => (l map ff, f(t), r map ff)
    })
  }

  def cojoin: TreeZipper[TreeZipper[A]] = {
    def unfoldStream[X, Y](seed: X)(f: X => Option[(Y, X)]): Stream[Y] =
      f(seed) match {
        case None => Stream.empty
        case Some((b, a)) => Stream.cons(b, unfoldStream(a)(f))
      }

    val lft = (_: TreeZipper[A]).left
    val rgt = (_: TreeZipper[A]).right
    def dwn[A](tz: TreeZipper[A]): (TreeZipper[A], () => Stream[TreeZipper[A]]) = {
      val f = () => unfoldStream(tz.firstChild) { o => for (c <- o) yield (c, c.right) }
      (tz, f)
    }
    def uf[A](a: TreeZipper[A], f: TreeZipper[A] => Option[TreeZipper[A]]): Stream[Tree[TreeZipper[A]]] = {
      unfoldStream(f(a)) { o => for (c <- o) yield (Tree.unfoldTree(c)(dwn[A](_: TreeZipper[A])), f(c)) }
    }

    val p = unfoldStream(parent) { o => for (z <- o) yield ((uf(z, lft), z, uf(z, rgt)), z.parent) }
    TreeZipper(Tree.unfoldTree(this)(dwn[A](_: TreeZipper[A])), uf(this, lft), uf(this, rgt), p)
  }

  private def downParents = (lefts, tree.rootLabel, rights) #:: parents
}

object TreeZipper {
  type TreeForest[A] = Stream[Tree[A]]

  type Parent[A] = (TreeForest[A], A, TreeForest[A])

  type Parents[A] = Stream[Parent[A]]

  private def combChildren[A](ls: Stream[A], t: A, rs: Stream[A]) =
    ls.foldLeft(t #:: rs)((a, b) => b #:: a)

  @tailrec
  private def splitChildren[A](acc: Stream[A], xs: Stream[A], n: Int): Option[(Stream[A], Stream[A])] =
    (acc, xs, n) match {
      case (acc, xs, 0) => Some((acc, xs))
      case (acc, Stream.cons(x, xs), n) => splitChildren(Stream.cons(x, acc), xs, n - 1)
      case _ => None
    }
}
