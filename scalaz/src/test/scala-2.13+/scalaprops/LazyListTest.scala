package scalaprops

import scalaz._
import scalaz.std.anyVal._
import ScalapropsScalaz._

object LazyListTest extends Scalaprops {

  // TODO define in scalaz-core
  private[this] implicit def lazyListEqual[A](implicit A: Equal[A]): Equal[LazyList[A]] = { (x, y) =>
    (x corresponds y)(A.equal)
  }

  // TODO define in scalaz-core
  private[this] implicit val lazyListInstance =
    new MonadPlus[LazyList]
      with BindRec[LazyList]
      with Align[LazyList]
      with Zip[LazyList]
      with IsEmpty[LazyList]
      with Cobind[LazyList]
      with Traverse[LazyList] {

      override def alignWith[A, B, C](f: A \&/ B => C) = { (a, b) =>
        if (b.isEmpty)
          a.map(x => f(\&/.This(x)))
        else if (a.isEmpty)
          b.map(x => f(\&/.That(x)))
        else
          f(\&/.Both(a.head, b.head)) #:: alignWith(f)(a.tail, b.tail)
      }

      override def point[A](a: => A) = a #:: LazyList.empty

      override def bind[A, B](fa: LazyList[A])(f: A => LazyList[B]) = fa flatMap f

      override def tailrecM[A, B](f: A => LazyList[A \/ B])(a: A) = {
        def go(s: LazyList[A \/ B]): LazyList[B] = {
          @annotation.tailrec
          def rec(abs: LazyList[A \/ B]): LazyList[B] =
            abs match {
              case \/-(b) #:: tail =>
                b #:: go(tail)
              case -\/(a) #:: tail =>
                rec(f(a) #::: tail)
              case _ =>
                LazyList.empty
            }
          rec(s)
        }
        go(f(a))
      }

      override def cojoin[A](a: LazyList[A]) = a.tails.to(LazyList).init

      override def cobind[A, B](fa: LazyList[A])(f: LazyList[A] => B) = map(cojoin(fa))(f)

      override def isEmpty[A](fa: LazyList[A]) = fa.isEmpty

      override def plus[A](a: LazyList[A], b: => LazyList[A]) = a #::: b

      override def empty[A] = LazyList.empty[A]

      override def traverseImpl[G[_], A, B](fa: LazyList[A])(f: A => G[B])(implicit G: Applicative[G]) = {
        val seed: G[LazyList[B]] = G.point(LazyList.empty[B])

        foldRight(fa, seed) { (x, ys) =>
          G.apply2(f(x), ys)((b, bs) => b #:: bs)
        }
      }

      override def foldRight[A, B](fa: LazyList[A], z: => B)(f: (A, => B) => B) = {
        if (fa.isEmpty)
          z
        else
          f(fa.head, foldRight(fa.tail, z)(f))
      }

      override def zip[A, B](a: => LazyList[A], b: => LazyList[B]) =
        a zip b
    }

  val bindRec = scalazlaws.bindRec.laws[LazyList].andThenParam(Param.maxSize(2))

  val laws = Properties.list(
    scalazlaws.monadPlusStrong.all[LazyList],
    scalazlaws.align.all[LazyList],
    scalazlaws.zip.all[LazyList],
    scalazlaws.isEmpty.all[LazyList],
    scalazlaws.cobind.all[LazyList],
    scalazlaws.traverse.all[LazyList]
  )

}
