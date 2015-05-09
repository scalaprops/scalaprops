package scalaprops

import Variant.variant
import scala.concurrent.Await
import scalaz._

abstract class Cogen[A] { self =>
  def cogen[B](a: A, g: Gen[B]): Gen[B]

  final def contramap[B](f: B => A): Cogen[B] =
    new Cogen[B] {
      def cogen[C](a: B, g: Gen[C]) =
        self.cogen(f(a), g)
    }
}

object Cogen extends CogenInstances {

  implicit val cogenBoolean: Cogen[Boolean] =
    new Cogen[Boolean] {
      def cogen[B](a: Boolean, g: Gen[B]) =
        variant(if(a) 0L else 1L, g)
    }

  implicit val cogenInt: Cogen[Int] =
    new Cogen[Int] {
      def cogen[B](a: Int, g: Gen[B]) =
        variant(if(a >= 0) 2 * a else -2 * a + 1, g)
    }

  implicit val cogenByte: Cogen[Byte] =
    new Cogen[Byte] {
      def cogen[B](a: Byte, g: Gen[B]) =
        variant(if(a >= 0) 2 * a else -2 * a + 1, g)
    }

  implicit val cogenShort: Cogen[Short] =
    new Cogen[Short] {
      def cogen[B](a: Short, g: Gen[B]) =
        variant(if(a >= 0) 2 * a else -2 * a + 1, g)
    }

  implicit val cogenLong: Cogen[Long] =
    new Cogen[Long] {
      def cogen[B](a: Long, g: Gen[B]) =
        variant(if(a >= 0L) 2L * a else -2L * a + 1L, g)
    }

  implicit val cogenChar: Cogen[Char] =
    new Cogen[Char] {
      def cogen[B](a: Char, g: Gen[B]) =
        variant(a << 1, g)
    }

  implicit val cogenFloat: Cogen[Float] =
    Cogen[Long].contramap(java.lang.Float.floatToIntBits)

  implicit val cogenDouble: Cogen[Double] =
    Cogen[Long].contramap(java.lang.Double.doubleToLongBits)

  implicit def cogenOption[A](implicit A: Cogen[A]): Cogen[Option[A]] =
    new Cogen[Option[A]] {
      def cogen[B](a: Option[A], g: Gen[B]) = a match {
        case Some(o) =>
          variant(1, A.cogen(o, g))
        case None =>
          variant(0, g)
      }
    }

  implicit def cogenMaybe[A: Cogen]: Cogen[Maybe[A]] =
    Cogen[Option[A]].contramap(_.toOption)

  implicit def cogenThese[A, B](implicit A: Cogen[A], B: Cogen[B]): Cogen[A \&/ B] =
    Cogen[(A \/ B) \/ (A, B)].contramap{
      case x @ \&/.Both(a, b) =>
        \/-((a, b))
      case \&/.This(a) =>
        -\/(-\/(a))
      case \&/.That(b) =>
        -\/(\/-(b))
    }

  implicit def cogenEither[A, B](implicit A: Cogen[A], B: Cogen[B]): Cogen[Either[A, B]] =
    new Cogen[Either[A, B]] {
      def cogen[Z](a: Either[A, B], g: Gen[Z]) = a match {
        case Right(x) =>
          variant(1, B.cogen(x, g))
        case Left(x) =>
          variant(0, A.cogen(x, g))
      }
    }

  implicit def cogenDisjunction[A, B](implicit A: Cogen[A], B: Cogen[B]): Cogen[A \/ B] =
    Cogen[Either[A, B]].contramap(_.toEither)


  implicit def cogenOneOr[F[_], A: Cogen](implicit F: Cogen[F[A]]): Cogen[OneOr[F, A]] =
    Cogen[F[A] \/ A].contramap(_.run)

  implicit def cogenIList[A](implicit A: Cogen[A]): Cogen[IList[A]] =
    new Cogen[IList[A]] {
      def cogen[B](a: IList[A], g: Gen[B]): Gen[B] = a match {
        case ICons(h, t) =>
          variant(1, A.cogen(h, cogenIList[A].cogen(t, g)))
        case INil() =>
          variant(0, g)
      }
    }

  implicit def cogenList[A: Cogen]: Cogen[List[A]] =
    Cogen[IList[A]].contramap(IList.fromList)

  implicit def cogenVector[A: Cogen]: Cogen[Vector[A]] = {
    import std.vector._
    Cogen[IList[A]].contramap(IList.fromFoldable(_))
  }

  implicit def cogenStream[A: Cogen]: Cogen[Stream[A]] = {
    import std.stream._
    Cogen[IList[A]].contramap(IList.fromFoldable(_))
  }

  implicit def cogenMap[A: Cogen, B: Cogen]: Cogen[Map[A, B]] =
    Cogen[IList[(A, B)]].contramap(
      _.iterator.foldLeft(IList.empty[(A, B)])(
        (list, keyValue) => keyValue :: list
      )
    )

  implicit def cogenSet[A: Cogen]: Cogen[Set[A]] =
    Cogen[List[A]].contramap(_.toList)

  implicit def cogenISet[A: Cogen]: Cogen[ISet[A]] =
    Cogen[List[A]].contramap(_.toList)

  implicit def cogenIMap[A: Cogen, B: Cogen]: Cogen[A ==>> B] =
    Cogen[List[(A, B)]].contramap(_.toList)

  implicit def cogenDList[A: Cogen]: Cogen[DList[A]] =
    Cogen[List[A]].contramap(_.toList)

  implicit def cogenDequeue[A: Cogen]: Cogen[Dequeue[A]] =
    Cogen[IList[A]].contramap(_.toIList)

  implicit def cogenNonEmptyList[A: Cogen]: Cogen[NonEmptyList[A]] =
    Cogen[(A, List[A])].contramap(nel => (nel.head, nel.tail))

  implicit val cogenString: Cogen[String] =
    new Cogen[String] {
      def cogen[B](a: String, g: Gen[B]) =
        cogenIList(cogenChar).cogen(IList(a.toCharArray: _*), g)
    }

  implicit def cogenCoproduct[F[_], G[_], A](implicit A: Cogen[F[A] \/ G[A]]): Cogen[Coproduct[F, G, A]] =
    Cogen[F[A] \/ G[A]].contramap(_.run)

  implicit def cogenZipper[A](implicit A: Cogen[A]): Cogen[Zipper[A]] =
    Cogen[(Stream[A], A, Stream[A])].contramap(z => (z.lefts, z.focus, z.rights))

  implicit def cogenIndexedStoreT[F[_], I: Cogen, A, B](implicit F: Cogen[F[A => B]]): Cogen[IndexedStoreT[F, I, A, B]] =
    Cogen[(F[A => B], I)].contramap(_.run)

  implicit def cogenIndexedContsT[W[_], M[_], R, O, A](implicit F: Cogen[W[A => M[O]] => M[R]]): Cogen[IndexedContsT[W, M, R, O, A]] =
    F.contramap(_.run)

  implicit def cogenKleisli[F[_], A, B](implicit F: Cogen[A => F[B]]): Cogen[Kleisli[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenCokleisli[F[_], A, B](implicit F: Cogen[F[A] => B]): Cogen[Cokleisli[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenNullResult[A: Gen, B: Cogen]: Cogen[NullResult[A, B]] =
    Cogen[A => Option[B]].contramap(_.apply)

  implicit def cogenNullArgument[A: Gen, B: Cogen]: Cogen[NullArgument[A, B]] =
    Cogen[Option[A] => B].contramap(_.apply)

  implicit def cogenEitherT[F[_], A, B](implicit F: Cogen[F[A \/ B]]): Cogen[EitherT[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenLazyEitherT[F[_], A, B](implicit F: Cogen[F[LazyEither[A, B]]]): Cogen[LazyEitherT[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenMaybeT[F[_], A](implicit F: Cogen[F[Maybe[A]]]): Cogen[MaybeT[F, A]] =
    F.contramap(_.run)

  implicit def cogenOptionT[F[_], A](implicit F: Cogen[F[Option[A]]]): Cogen[OptionT[F, A]] =
    F.contramap(_.run)

  implicit def cogenLazyOptionT[F[_], A](implicit F: Cogen[F[LazyOption[A]]]): Cogen[LazyOptionT[F, A]] =
    F.contramap(_.run)

  implicit def cogenIdT[F[_], A](implicit F: Cogen[F[A]]): Cogen[IdT[F, A]] =
    F.contramap(_.run)

  implicit def cogenIndexedReaderWriterStateT[F[_], R, W, S1, S2, A](implicit F: Cogen[(R, S1) => F[(W, A, S2)]]): Cogen[IndexedReaderWriterStateT[F, R, W, S1, S2, A]] =
    F.contramap(_.run)

  implicit def cogenWriterT[F[_], A, B](implicit F: Cogen[F[(A, B)]]): Cogen[WriterT[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenUnwriterT[F[_], A, B](implicit F: Cogen[F[(A, B)]]): Cogen[UnwriterT[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenTree[A: Cogen]: Cogen[Tree[A]] =
    Cogen[(A, Stream[Tree[A]])].contramap(t => (t.rootLabel, t.subForest))

  implicit def cogenCoyoneda[F[_]: Functor, A](implicit F: Cogen[F[A]]): Cogen[Coyoneda[F, A]] =
    F.contramap(_.run)

  implicit def cogenFuture[A](implicit F: Cogen[A]): Cogen[scala.concurrent.Future[A]] = {
    import scala.concurrent.duration._
    F.contramap(f => Await.result(f, 5.seconds))
  }

  implicit val instance: Contravariant[Cogen] =
    new Contravariant[Cogen] {
      def contramap[A, B](r: Cogen[A])(f: B => A) =
        r contramap f
    }

  def apply[A](implicit A: Cogen[A]): Cogen[A] = A
}
