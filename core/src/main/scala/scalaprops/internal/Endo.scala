package scalaprops
package internal

final case class Endo[A](run: A => A) {
  def apply(a: A): A = run(a)

  def compose(other: Endo[A]): Endo[A] = Endo(run compose other.run)

  def andThen(other: Endo[A]): Endo[A] = other compose this
}

object Endo {
  private[this] val id: Endo[Any] = Endo[Any](a => a)
  def idEndo[A]: Endo[A] = id.asInstanceOf[Endo[A]]
}
