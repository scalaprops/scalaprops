package scalaprops
package internal

sealed abstract class Tree[A] {
  import Tree._

  def rootLabel: A

  def subForest: Stream[Tree[A]]

  def zipper: TreeZipper[A] = TreeZipper(this, Stream.Empty, Stream.Empty, Stream.Empty)

  def map[B](f: A => B): Tree[B] =
    Node(f(rootLabel), subForest map (_ map f))

  private def traverse[S, B](f: A => State[S, B]): State[S, Tree[B]] = {
    def traverseStream[S, A, B](fa: Stream[A])(f: A => State[S, B]): State[S, Stream[B]] = {
      val seed = State[S, Stream[B]](s => (s, Stream.empty[B]))

      fa.foldRight(seed) { (x, ys) =>
        for {
          b <- f(x)
          bs <- ys
        } yield b #:: bs
      }
    }

    subForest match {
      case Stream.Empty =>
        f(rootLabel).map(Leaf(_))
      case xs =>
        for {
          h <- f(rootLabel)
          t <- traverseStream(xs)(_.traverse(f))
        } yield Node(h, t)
    }
  }

  private[this] def traverseS[S, B](s: S)(f: A => State[S, B]): (S, Tree[B]) = {
    val x = traverse(f).run(s)
    (x._1, x._2)
  }

  def mapAccumL[S, B](z: S)(f: (S, A) => (S, B)): (S, Tree[B]) =
    traverseS(z)(a =>
      for {
        s1 <- State.init[S]
        (s2, b) = f(s1, a)
        _ <- State.put(s2)
      } yield b
    )

  private def draw: Stream[String] = {
    def drawSubTrees(s: Stream[Tree[A]]): Stream[String] =
      s match {
        case Stream.Empty => Stream.Empty
        case Stream(t) => "|" #:: shift("`- ", "   ", t.draw)
        case t #:: ts => "|" #:: shift("+- ", "|  ", t.draw) append drawSubTrees(ts)
      }
    def shift(first: String, other: String, s: Stream[String]): Stream[String] =
      (first #:: Stream.continually(other)).zip(s).map { case (a, b) =>
        a + b
      }

    rootLabel.toString #:: drawSubTrees(subForest)
  }

  def drawTree: String =
    draw.map(_ + "\n").mkString
}

object Tree {
  object Node {
    def apply[A](root: => A, forest: => Stream[Tree[A]]): Tree[A] = {
      new Tree[A] {
        private[this] val rootc = Lazy(root)
        private[this] val forestc = Lazy(forest)
        def rootLabel = rootc.value
        def subForest = forestc.value

        override def toString = "<tree>"
      }
    }

    def unapply[A](t: Tree[A]): Option[(A, Stream[Tree[A]])] = Some((t.rootLabel, t.subForest))
  }

  object Leaf {
    def apply[A](root: => A): Tree[A] = {
      Node(root, Stream.empty)
    }

    def unapply[A](t: Tree[A]): Option[A] = {
      t match {
        case Node(root, Stream.Empty) =>
          Some(root)
        case _ =>
          None
      }
    }
  }

  def unfoldForest[A, B](s: Stream[A])(f: A => (B, () => Stream[A])): Stream[Tree[B]] =
    s.map(unfoldTree(_)(f))

  def unfoldTree[A, B](v: A)(f: A => (B, () => Stream[A])): Tree[B] =
    f(v) match {
      case (a, bs) => Node(a, unfoldForest(bs.apply())(f))
    }

  private final case class State[S, A](run: S => (S, A)) {
    def map[B](f: A => B): State[S, B] =
      State(run.andThen { case (s, a) => (s, f(a)) })

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State[S, B] { s0 =>
        val (s1, a) = run(s0)
        f(a).run(s1)
      }
  }

  private object State {
    def init[S]: State[S, S] = State(s => (s, s))

    def put[S](s: S): State[S, Unit] = State(_ => (s, ()))
  }
}
