package scalaprops

import scalaz._

abstract class Eq1[F[_]] extends Serializable { self =>

  def eq1[A](implicit A: Equal[A]): Equal[F[A]]

  final def cohoist[G[_]](f: G ~> F): Eq1[G] =
    new Eq1[G] {
      def eq1[A](implicit A: Equal[A]) =
        self.eq1(A).contramap(f.apply[A])
    }
}

object Eq1 {
  @inline def apply[F[_]](implicit F: Eq1[F]): Eq1[F] = F

  implicit val list: Eq1[List] =
    new Eq1[List] {
      def eq1[A](implicit A: Equal[A]) =
        scalaz.std.list.listEqual(A)
    }

  implicit val iList: Eq1[IList] =
    new Eq1[IList] {
      def eq1[A](implicit A: Equal[A]) =
        Equal[IList[A]]
    }

  implicit val option: Eq1[Option] =
    new Eq1[Option] {
      def eq1[A](implicit A: Equal[A]) =
        scalaz.std.option.optionEqual(A)
    }

  implicit val maybe: Eq1[Maybe] =
    new Eq1[Maybe] {
      def eq1[A](implicit A: Equal[A]) =
        Equal[Maybe[A]]
    }

  implicit def streamT[F[_]: Monad](implicit F: Eq1[F]): Eq1[({type l[a] = StreamT[F, a]})#l] =
    new Eq1[({type l[a] = StreamT[F, a]})#l] {
      def eq1[A](implicit A: Equal[A]) = {
        import scalaz.std.stream._
        implicit val e = F.eq1[Stream[A]]
        StreamT.StreamTEqual[F, A]
      }
    }

  implicit def maybeT[F[_]](implicit F: Eq1[F]): Eq1[({type l[a] = MaybeT[F, a]})#l] =
    new Eq1[({type l[a] = MaybeT[F, a]})#l] {
      def eq1[A](implicit A: Equal[A]) = {
        MaybeT.maybeTEqual(F.eq1[Maybe[A]])
      }
    }

  implicit def eitherT[F[_], B: Equal](implicit F: Eq1[F]): Eq1[({type l[a] = EitherT[F, B, a]})#l] =
    new Eq1[({type l[a] = EitherT[F, B, a]})#l] {
      def eq1[A](implicit A: Equal[A]) = {
        EitherT.eitherTEqual(F.eq1[B \/ A])
      }
    }

  implicit def writerT[F[_], B: Equal](implicit F: Eq1[F]): Eq1[({type l[a] = WriterT[F, B, a]})#l] =
    new Eq1[({type l[a] = WriterT[F, B, a]})#l] {
      import scalaz.std.tuple._
      def eq1[A](implicit A: Equal[A]) =
        WriterT.writerTEqual(F.eq1[(B, A)])
    }
}
