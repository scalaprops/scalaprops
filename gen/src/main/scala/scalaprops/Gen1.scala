package scalaprops

import scalaz._

abstract class Gen1[F[_]] extends Serializable { self =>
  def gen1[A](implicit A: Gen[A]): Gen[F[A]]

  final def trans[G[_]](f: F ~> G): Gen1[G] =
    new Gen1[G] {
      def gen1[A](implicit A: Gen[A]) =
        self.gen1(A).map(f.apply[A])
    }
}

object Gen1 {

  @inline def apply[F[_]](implicit F: Gen1[F]): Gen1[F] = F

  implicit def kleisliGen1[F[_], B](implicit F: Gen1[F], B: Cogen[B]): Gen1[({type l[a] = Kleisli[F, B, a]})#l] =
    new Gen1[({type l[a] = Kleisli[F, B, a]})#l] {
      def gen1[A](implicit A: Gen[A]) = {
        implicit val g = F.gen1(A)
        Gen.kleisli[F, B, A]
      }
    }

  implicit val optionGen1: Gen1[Option] =
    new Gen1[Option] {
      def gen1[A](implicit A: Gen[A]) =
        Gen.option(A)
    }

  implicit val maybeGen1: Gen1[Maybe] =
    new Gen1[Maybe] {
      def gen1[A](implicit A: Gen[A]) =
        Gen.maybe(A)
    }

  implicit val listGen1: Gen1[List] =
    new Gen1[List] {
      def gen1[A](implicit A: Gen[A]) =
        Gen.list(A)
    }

  implicit val iListGen1: Gen1[IList] =
    new Gen1[IList] {
      def gen1[A](implicit A: Gen[A]) =
        Gen.ilist(A)
    }

  implicit def iMapGen1[A: Order: Gen]: Gen1[({type l[a] = A ==>> a})#l] =
    new Gen1[({type l[a] = A ==>> a})#l] {
      def gen1[B](implicit B: Gen[B]) =
        Gen.imapGen[A, B]
    }

  implicit def mapGen1[A: Gen]: Gen1[({type l[a] = Map[A, a]})#l] =
    new Gen1[({type l[a] = Map[A, a]})#l] {
      def gen1[B](implicit B: Gen[B]) =
        Gen.mapGen[A, B]
    }

  implicit val setGen1: Gen1[Set] =
    new Gen1[Set] {
      def gen1[A](implicit A: Gen[A]) =
        Gen.setGen(A)
    }

  implicit def streamTGen1[F[_]: Applicative](implicit F: Gen1[F]): Gen1[({type l[a] = StreamT[F, a]})#l] =
    new Gen1[({type l[a] = StreamT[F, a]})#l] {
      def gen1[A](implicit A: Gen[A]) = {
        implicit val g = F.gen1[Stream[A]]
        Gen.streamTGen[F, A]
      }
    }

  implicit def maybeTGen1[F[_]](implicit F: Gen1[F]): Gen1[({type l[a] = MaybeT[F, a]})#l] =
    new Gen1[({type l[a] = MaybeT[F, a]})#l] {
      def gen1[A](implicit A: Gen[A]) = {
        Gen.maybeTGen(F.gen1[Maybe[A]])
      }
    }

  implicit def eitherTGen1[F[_], B: Gen](implicit F: Gen1[F]): Gen1[({type l[a] = EitherT[F, B, a]})#l] =
    new Gen1[({type l[a] = EitherT[F, B, a]})#l] {
      def gen1[A](implicit A: Gen[A]) = {
        Gen.eitherTGen(F.gen1[B \/ A])
      }
    }

  implicit def writerTGen1[F[_], B: Gen](implicit F: Gen1[F]): Gen1[({type l[a] = WriterT[F, B, a]})#l] =
    new Gen1[({type l[a] = WriterT[F, B, a]})#l] {
      def gen1[A](implicit A: Gen[A]) =
        Gen.writerTGen(F.gen1[(B, A)])
    }

  implicit val vectorGen1: Gen1[Vector] =
    new Gen1[Vector] {
      def gen1[A: Gen] =
        Gen[Vector[A]]
    }

  implicit val streamGen1: Gen1[Stream] =
    new Gen1[Stream] {
      def gen1[A: Gen] =
        Gen[Stream[A]]
    }

  implicit def validationGen1[E: Gen]: Gen1[({type l[a] = Validation[E, a]})#l] =
    new Gen1[({type l[a] = Validation[E, a]})#l] {
      def gen1[A: Gen] =
        Gen[Validation[E, A]]
    }

  implicit def disjunctionGen1[E: Gen]: Gen1[({type l[a] = E \/ a})#l] =
    new Gen1[({type l[a] = E \/ a})#l] {
      def gen1[A: Gen] =
        Gen[E \/ A]
    }

  implicit val nonEmptyListGen1: Gen1[NonEmptyList] =
    new Gen1[NonEmptyList] {
      def gen1[A: Gen] =
        Gen[NonEmptyList[A]]
    }
}
