package scalaprops

import scalaz._
import ScalapropsScalaz._

abstract class Gen1[F[_]] { self =>

  def gen1[A: Gen]: Gen[F[A]]

  final def mapF[G[_]](f: F ~> G): Gen1[G] =
    new Gen1[G] {
      def gen1[A: Gen]: Gen[G[A]] =
        self.gen1[A].map(f.apply)
    }
}

object Gen1 {

  @inline def apply[F[_]](implicit F: Gen1[F]): Gen1[F] = F

  implicit val list: Gen1[List] =
    new Gen1[List] {
      def gen1[A: Gen] =
        Gen[List[A]]
    }

  implicit val iList: Gen1[IList] =
    new Gen1[IList] {
      def gen1[A: Gen] =
        Gen[IList[A]]
    }

  implicit val vector: Gen1[Vector] =
    new Gen1[Vector] {
      def gen1[A: Gen] =
        Gen[Vector[A]]
    }

  implicit val stream: Gen1[Stream] =
    new Gen1[Stream] {
      def gen1[A: Gen] =
        Gen[Stream[A]]
    }

  implicit val maybe: Gen1[Maybe] =
    new Gen1[Maybe] {
      def gen1[A: Gen] =
        Gen[Maybe[A]]
    }

  implicit def validation[E: Gen]: Gen1[({ type l[a] = Validation[E, a] })#l] =
    new Gen1[({ type l[a] = Validation[E, a] })#l] {
      def gen1[A: Gen] =
        Gen[Validation[E, A]]
    }

  implicit def disjunction[E: Gen]: Gen1[({ type l[a] = E \/ a })#l] =
    new Gen1[({ type l[a] = E \/ a })#l] {
      def gen1[A: Gen] =
        Gen[E \/ A]
    }

  implicit val option: Gen1[Option] =
    Gen1[Maybe].mapF(Maybe.optionMaybeIso.from)

  implicit val nonEmptyList: Gen1[NonEmptyList] =
    new Gen1[NonEmptyList] {
      def gen1[A: Gen] =
        Gen[NonEmptyList[A]]
    }

}
