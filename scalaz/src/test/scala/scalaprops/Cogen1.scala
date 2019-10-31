package scalaprops

import scalaz._
import ScalapropsScalaz._

abstract class Cogen1[F[_]] {
  def cogen1[A: Cogen]: Cogen[F[A]]
}

object Cogen1 {
  @inline def apply[F[_]](implicit F: Cogen1[F]): Cogen1[F] = F

  implicit val maybe: Cogen1[Maybe] =
    new Cogen1[Maybe] {
      def cogen1[A: Cogen] =
        Cogen[Maybe[A]]
    }

  implicit def validation[B: Cogen]: Cogen1[({ type l[a] = Validation[B, a] })#l] =
    new Cogen1[({ type l[a] = Validation[B, a] })#l] {
      def cogen1[A: Cogen] =
        Cogen[Validation[B, A]]
    }

  implicit def disjunction[B: Cogen]: Cogen1[({ type l[a] = B \/ a })#l] =
    new Cogen1[({ type l[a] = B \/ a })#l] {
      def cogen1[A: Cogen] =
        Cogen[B \/ A]
    }

  implicit val stream: Cogen1[Stream] =
    new Cogen1[Stream] {
      def cogen1[A: Cogen] =
        Cogen[Stream[A]]
    }

  implicit val list: Cogen1[List] =
    new Cogen1[List] {
      def cogen1[A: Cogen] =
        Cogen[List[A]]
    }

  implicit val iList: Cogen1[IList] =
    new Cogen1[IList] {
      def cogen1[A: Cogen] =
        Cogen[IList[A]]
    }
}
