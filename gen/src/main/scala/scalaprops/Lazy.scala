package scalaprops

final class Lazy[A] private (private var a: () => A) {
  lazy val value: A = {
    val x = a()
    a = null
    x
  }

  override def equals(other: Any): Boolean =
    other match {
      case that: Lazy[_] =>
        value == that.value
      case _ =>
        false
    }

  override def hashCode: Int =
    value.##

  def map[B](f: A => B): Lazy[B] =
    new Lazy[B](() => f(value))

  def flatMap[B](f: A => Lazy[B]): Lazy[B] =
    f(value)
}

object Lazy {
  def apply[A](a: => A): Lazy[A] = new Lazy(() => a)
}
