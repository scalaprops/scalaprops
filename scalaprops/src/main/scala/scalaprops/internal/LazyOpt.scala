package scalaprops
package internal

sealed abstract class LazyOpt[A] extends Product with Serializable {

  import LazyOpt._

  def fold[X](some: (=> A) => X, none: => X): X =
    this match {
      case s @ LazySome(_) =>
        some(s.value)
      case LazyNone() =>
        none
    }

  def getOrElse(default: => A): A =
    fold(a => a, default)

  def map[B](f: (=> A) => B): LazyOpt[B] =
    fold(a => lazySome(f(a)), lazyNone)
}

object LazyOpt {

  private final case class LazySome[A](private var a: () => A) extends LazyOpt[A] {
    lazy val value: A = {
      val x = a()
      a = null
      x
    }
  }

  private final case class LazyNone[A] () extends LazyOpt[A]

  private[this] val none = LazyNone[Any]

  def lazySome[A](a: => A): LazyOpt[A] =
    LazySome(() => a)

  def lazyNone[A]: LazyOpt[A] =
    none.asInstanceOf[LazyOpt[A]]

  def fromOption[A](oa: Option[A]): LazyOpt[A] = oa match {
    case Some(x) => lazySome(x)
    case None    => lazyNone[A]
  }
}
