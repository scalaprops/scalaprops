package scalaprops

final case class Bool private(b: Boolean) {
  def is = b
  def isNot = !is
  def implies(p: => Property): Property =
    Property.implies(b, p)
  def implies(c: Bool): Property =
    implies(c.b)
  def implies(c: Boolean): Property =
    implies(Property.prop(b))
}

object Bool {
  private[this] val t = Bool(true)
  private[this] val f = Bool(false)

  def bool(b: Boolean): Bool =
    if(b) t else f
}
