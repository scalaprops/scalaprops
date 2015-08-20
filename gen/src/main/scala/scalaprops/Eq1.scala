package scalaprops

import scalaz._
import scalaz.std.AllInstances._

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

  implicit val listEq1: Eq1[List] =
    new Eq1[List] {
      def eq1[A](implicit A: Equal[A]) =
        scalaz.std.list.listEqual(A)
    }

  implicit val iListEq1: Eq1[IList] =
    new Eq1[IList] {
      def eq1[A](implicit A: Equal[A]) =
        Equal[IList[A]]
    }

  implicit val optionEq1: Eq1[Option] =
    new Eq1[Option] {
      def eq1[A](implicit A: Equal[A]) =
        scalaz.std.option.optionEqual(A)
    }

  implicit val maybeEq1: Eq1[Maybe] =
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

  implicit def freeEq1[F[_]: Functor](implicit F: Eq1[F]): Eq1[({type l[a] = Free[F, a]})#l] =
    new Eq1[({type l[a] = Free[F, a]})#l] {
      def eq1[A: Equal] = new Equal[Free[F, A]] {
        def equal(a: Free[F, A], b: Free[F, A]) = {
          implicit val s: Equal[F[Free[F, A]]] = F.eq1[Free[F, A]](freeEq1[F].eq1)
          Equal[F[Free[F, A]] \/ A].equal(a.resume, b.resume)
        }
      }
    }

  implicit val treeEq1: Eq1[Tree] =
    new Eq1[Tree] {
      def eq1[A: Equal] = Equal[Tree[A]]
    }

  implicit val streamEq1: Eq1[Stream] =
    new Eq1[Stream] {
      def eq1[A: Equal] = Equal[Stream[A]]
    }

  implicit val nelEq1: Eq1[NonEmptyList] =
    new Eq1[NonEmptyList] {
      def eq1[A: Equal] = Equal[NonEmptyList[A]]
    }

  implicit def writerTEq1[F[_], W](implicit F: Eq1[F], W: Equal[W]): Eq1[({type l[a] = WriterT[F, W, a]})#l] =
    new Eq1[({type l[a] = WriterT[F, W, a]})#l] {
      def eq1[A: Equal] = {
        implicit val e = F.eq1[(W, A)]
        Equal[WriterT[F, W, A]]
      }
    }

  implicit def disjunctionEq1[A: Equal]: Eq1[({type l[a] = A \/ a})#l] =
    new Eq1[({type l[a] = A \/ a})#l] {
      def eq1[B: Equal] = Equal[A \/ B]
    }

  implicit def validationEq1[A: Equal]: Eq1[({type l[a] = Validation[A, a]})#l] =
    new Eq1[({type l[a] = Validation[A, a]})#l] {
      def eq1[B: Equal] = Equal[Validation[A, B]]
    }

  implicit val idEq1: Eq1[Id.Id] =
    new Eq1[Id.Id] {
      def eq1[A: Equal] = Equal[A]
    }
}
