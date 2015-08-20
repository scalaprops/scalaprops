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

  implicit def kleisli[F[_], B](implicit F: Gen1[F], B: Cogen[B]): Gen1[({type l[a] = Kleisli[F, B, a]})#l] =
    new Gen1[({type l[a] = Kleisli[F, B, a]})#l] {
      def gen1[A](implicit A: Gen[A]) = {
        implicit val g = F.gen1(A)
        Gen.kleisli[F, B, A]
      }
    }

  implicit val option: Gen1[Option] =
    new Gen1[Option] {
      def gen1[A](implicit A: Gen[A]) =
        Gen.option(A)
    }

  implicit val maybe: Gen1[Maybe] =
    new Gen1[Maybe] {
      def gen1[A](implicit A: Gen[A]) =
        Gen.maybe(A)
    }

  implicit val list: Gen1[List] =
    new Gen1[List] {
      def gen1[A](implicit A: Gen[A]) =
        Gen.list(A)
    }

  implicit val iList: Gen1[IList] =
    new Gen1[IList] {
      def gen1[A](implicit A: Gen[A]) =
        Gen.ilist(A)
    }

  implicit def iMap[A: Order: Gen]: Gen1[({type l[a] = A ==>> a})#l] =
    new Gen1[({type l[a] = A ==>> a})#l] {
      def gen1[B](implicit B: Gen[B]) =
        Gen.imapGen[A, B]
    }

  implicit def iMap[A: Gen]: Gen1[({type l[a] = Map[A, a]})#l] =
    new Gen1[({type l[a] = Map[A, a]})#l] {
      def gen1[B](implicit B: Gen[B]) =
        Gen.mapGen[A, B]
    }

  implicit val set: Gen1[Set] =
    new Gen1[Set] {
      def gen1[A](implicit A: Gen[A]) =
        Gen.setGen(A)
    }

  implicit def streamT[F[_]: Applicative](implicit F: Gen1[F]): Gen1[({type l[a] = StreamT[F, a]})#l] =
    new Gen1[({type l[a] = StreamT[F, a]})#l] {
      def gen1[A](implicit A: Gen[A]) = {
        implicit val g = F.gen1[Stream[A]]
        Gen.streamTGen[F, A]
      }
    }

  implicit def maybeT[F[_]](implicit F: Gen1[F]): Gen1[({type l[a] = MaybeT[F, a]})#l] =
    new Gen1[({type l[a] = MaybeT[F, a]})#l] {
      def gen1[A](implicit A: Gen[A]) = {
        Gen.maybeTGen(F.gen1[Maybe[A]])
      }
    }

  implicit def eitherT[F[_], B: Gen](implicit F: Gen1[F]): Gen1[({type l[a] = EitherT[F, B, a]})#l] =
    new Gen1[({type l[a] = EitherT[F, B, a]})#l] {
      def gen1[A](implicit A: Gen[A]) = {
        Gen.eitherTGen(F.gen1[B \/ A])
      }
    }

  implicit def writerT[F[_], B: Gen](implicit F: Gen1[F]): Gen1[({type l[a] = WriterT[F, B, a]})#l] =
    new Gen1[({type l[a] = WriterT[F, B, a]})#l] {
      def gen1[A](implicit A: Gen[A]) =
        Gen.writerTGen(F.gen1[(B, A)])
    }
}
