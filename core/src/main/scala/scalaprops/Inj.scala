package scalaprops

sealed abstract class Inj[C <: Or, I] extends Serializable {
  def apply(i: I): C
}

object Inj {
  def apply[C <: Or, I](implicit inject: Inj[C, I]): Inj[C, I] = inject

  implicit def tlInj[H, T <: Or, I](implicit tlInj: Inj[T, I]): Inj[H :-: T, I] = new Inj[H :-: T, I] {
    def apply(i: I): H :-: T = Or.R(tlInj(i))
  }

  implicit def hdInj[H, T <: Or]: Inj[H :-: T, H] = new Inj[H :-: T, H] {
    def apply(i: H): H :-: T = Or.L(i)
  }
}
