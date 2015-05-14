package scalaprops

import scalaz._
import Or.{L, R}

sealed abstract class Or

sealed abstract class :-:[+H, +T <: Or] extends Or

sealed abstract class OrConsInstances {

  implicit def order[H, T <: Or](implicit H: Order[H], T: Order[T]): Order[H :-: T] =
    Order.order{
      case (L(a), L(b)) =>
        H.order(a, b)
      case (R(a), R(b)) =>
        T.order(a, b)
      case (R(_), L(_)) =>
        Ordering.LT
      case (L(_), R(_)) =>
        Ordering.GT
    }

}

object :-: extends OrConsInstances {

  implicit def equal[H, T <: Or](implicit H: Equal[H], T: Equal[T]): Equal[H :-: T] =
    Equal.equal{
      case (L(a), L(b)) =>
        H.equal(a, b)
      case (R(a), R(b)) =>
        T.equal(a, b)
      case _ =>
        false
    }

}

object Or {

  final case class L[+H, +T <: Or](head : H) extends :-:[H, T] {
    override def toString = head.toString
  }

  final case class R[+H, +T <: Or](tail : T) extends :-:[H, T] {
    override def toString = tail.toString
  }

  sealed trait Empty extends Or

  object Empty {
    implicit val instance: Order[Empty] =
      Order.order((_, _) => Ordering.EQ)
  }

  final class MkOr[C <: Or] private[Or] {
    def apply[T](t: T)(implicit inj: Inj[C, T]): C = inj(t)
  }

  def apply[C <: Or]: MkOr[C] = new MkOr[C]

}
