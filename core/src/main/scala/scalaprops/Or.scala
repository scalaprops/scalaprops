package scalaprops

sealed abstract class Or

sealed abstract class :-:[+H, +T <: Or] extends Or

sealed abstract class OrConsInstances {}

object :-: extends OrConsInstances {
  implicit def orGen[A, B <: Or](implicit A: Gen[A], B: Gen[B]): Gen[A :-: B] = {
    if B eq Or.Empty.orEmptyGen then {
      A.map[A :-: B](Or.L(_))
    } else {
      Gen.frequency(
        1 -> A.map[A :-: B](Or.L(_)),
        3 -> B.map[A :-: B](Or.R(_))
      )
    }
  }
}

object Or {
  final case class L[+H, +T <: Or](head: H) extends :-:[H, T] {
    override def toString = head.toString
  }

  final case class R[+H, +T <: Or](tail: T) extends :-:[H, T] {
    override def toString = tail.toString
  }

  sealed trait Empty extends Or

  object Empty {
    implicit val orEmptyGen: Gen[Or.Empty] =
      Gen.oneOfLazy(Lazy(???))
  }

  final class MkOr[C <: Or] private[Or] {
    def apply[T](t: T)(implicit inj: Inj[C, T]): C = inj(t)
    private[scalaprops] def _apply[T](implicit inj: Inj[C, T]): T => C = inj(_)
  }

  def apply[C <: Or]: MkOr[C] = new MkOr[C]
}
