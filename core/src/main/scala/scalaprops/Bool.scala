package scalaprops

final case class Bool private (b: Boolean) {
  def is = b
  def isNot = !is
  def implies[A](p: => A)(implicit A: AsProperty[A]): Property =
    Property.implies(b, A.asProperty(p))
}

object Bool {
  private[this] val t = Bool(true)
  private[this] val f = Bool(false)

  def bool(b: Boolean): Bool =
    if b then t else f
}
