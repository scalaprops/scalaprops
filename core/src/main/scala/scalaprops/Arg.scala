package scalaprops

sealed abstract class Arg {
  type A
  val value: A
  val shrinks: Int
  override final def toString = s"Arg(${shrinks}, ${value})"
}

object Arg {
  def apply[A0](value0: A0, shrinks0: Int): Arg =
    new Arg {
      type A = A0
      val value = value0
      val shrinks = shrinks0
    }
}
