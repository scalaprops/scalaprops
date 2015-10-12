package scalaprops

import Variant.variant
import scala.concurrent.Await
import scalaz._

abstract class Cogen[A] { self =>
  def cogen[B](a: A, g: CogenState[B]): CogenState[B]

  final def contramap[B](f: B => A): Cogen[B] =
    new Cogen[B] {
      def cogen[C](a: B, g: CogenState[C]) =
        self.cogen(f(a), g)
    }

  final def naturalTrans: ({type l[a] = (A, CogenState[a])})#l ~> CogenState =
    new (({type l[a] = (A, CogenState[a])})#l ~> CogenState) {
      def apply[C](fa: (A, CogenState[C])) =
        self.cogen(fa._1, fa._2)
    }
}

sealed abstract class CogenInstances0 extends CogenInstances {

  implicit final def cogenEndomorphic[F[_, _], A](implicit F: Cogen[F[A, A]]): Cogen[Endomorphic[F, A]] =
    F.contramap(_.run)

}

object Cogen extends CogenInstances0 {

  implicit def f1[A1, Z](implicit A1: Gen[A1], C: Cogen[Z]): Cogen[A1 => Z] =
    new Cogen[A1 => Z] {
      def cogen[X](f: A1 => Z, g: CogenState[X]) =
        CogenState(g.rand.next, A1.flatMap(x => C.cogen(f(x), g).gen))
    }

  def from[A1, Z](f: Z => Option[A1])(implicit A1: Cogen[A1]): Cogen[Z] =
    from1[A1, Z](f)(A1)

  def from1[A1, Z](f: Z => Option[A1])(implicit A1: Cogen[A1]): Cogen[Z] =
    A1.contramap(t => f(t).get)

  implicit val cogenBoolean: Cogen[Boolean] =
    new Cogen[Boolean] {
      def cogen[B](a: Boolean, g: CogenState[B]) =
        variant(if(a) 0L else 1L, g)
    }

  implicit val cogenUnit: Cogen[Unit] =
    new Cogen[Unit] {
      def cogen[B](a: Unit, g: CogenState[B]) = g
    }

  implicit val cogenInt: Cogen[Int] =
    new Cogen[Int] {
      def cogen[B](a: Int, g: CogenState[B]) =
        variant(if(a >= 0) 2 * a else -2 * a + 1, g)
    }

  implicit val cogenByte: Cogen[Byte] =
    new Cogen[Byte] {
      def cogen[B](a: Byte, g: CogenState[B]) =
        variant(if(a >= 0) 2 * a else -2 * a + 1, g)
    }

  implicit val cogenShort: Cogen[Short] =
    new Cogen[Short] {
      def cogen[B](a: Short, g: CogenState[B]) =
        variant(if(a >= 0) 2 * a else -2 * a + 1, g)
    }

  implicit val cogenLong: Cogen[Long] =
    new Cogen[Long] {
      def cogen[B](a: Long, g: CogenState[B]) =
        variant(if(a >= 0L) 2L * a else -2L * a + 1L, g)
    }

  implicit val cogenChar: Cogen[Char] =
    new Cogen[Char] {
      def cogen[B](a: Char, g: CogenState[B]) =
        variant(a << 1, g)
    }

  implicit val cogenFloat: Cogen[Float] =
    Cogen[Long].contramap(java.lang.Float.floatToIntBits)

  implicit val cogenDouble: Cogen[Double] =
    Cogen[Long].contramap(java.lang.Double.doubleToLongBits)

  implicit val cogenJavaBoolean: Cogen[java.lang.Boolean] =
    Cogen[Boolean].contramap(_.booleanValue)

  implicit val cogenJavaInteger: Cogen[java.lang.Integer] =
    Cogen[Int].contramap(_.intValue)

  implicit val cogenJavaByte: Cogen[java.lang.Byte] =
    Cogen[Byte].contramap(_.byteValue)

  implicit val cogenJavaShort: Cogen[java.lang.Short] =
    Cogen[Short].contramap(_.shortValue)

  implicit val cogenJavaLong: Cogen[java.lang.Long] =
    Cogen[Long].contramap(_.longValue)

  implicit val cogenJavaCharacter: Cogen[java.lang.Character] =
    Cogen[Char].contramap(_.charValue)

  implicit val cogenJavaFloat: Cogen[java.lang.Float] =
    Cogen[Float].contramap(_.floatValue)

  implicit val cogenJavaDouble: Cogen[java.lang.Double] =
    Cogen[Double].contramap(_.doubleValue)

  import java.{ math => jm }

  implicit val cogenBigInteger: Cogen[jm.BigInteger] =
    Cogen[Array[Byte]].contramap(_.toByteArray)

  implicit val cogenBigInt: Cogen[BigInt] =
    Cogen[Array[Byte]].contramap(_.toByteArray)

  implicit val cogenJavaBigDecimal: Cogen[jm.BigDecimal] =
    new Cogen[jm.BigDecimal] {
      def cogen[B](a: jm.BigDecimal, g: CogenState[B]) =
        Cogen[jm.BigInteger].cogen(a.unscaledValue, Cogen[Int].cogen(a.scale, g))
    }

  implicit val cogenBigDecimal: Cogen[BigDecimal] =
    Cogen[jm.BigDecimal].contramap(_.bigDecimal)

  implicit def cogenOption[A](implicit A: Cogen[A]): Cogen[Option[A]] =
    new Cogen[Option[A]] {
      def cogen[B](a: Option[A], g: CogenState[B]) = a match {
        case Some(o) =>
          variant(1, A.cogen(o, g))
        case None =>
          variant(g.rand.nextInt._2, g)
      }
    }

  implicit def cogenMaybe[A: Cogen]: Cogen[Maybe[A]] =
    Cogen[Option[A]].contramap(_.toOption)

  implicit def cogenLazyOption[A: Cogen]: Cogen[LazyOption[A]] =
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
      def cogen[Z](a: Either[A, B], g: CogenState[Z]) = a match {
        case Right(x) =>
          variant(1, B.cogen(x, g))
        case Left(x) =>
          variant(0, A.cogen(x, g.copy(rand = g.rand.next)))
      }
    }

  implicit def cogenLazyEither[A: Cogen, B: Cogen]: Cogen[LazyEither[A, B]] =
    Cogen[Either[A, B]].contramap(_.toEither)

  implicit def cogenDisjunction[A, B](implicit A: Cogen[A], B: Cogen[B]): Cogen[A \/ B] =
    Cogen[Either[A, B]].contramap(_.toEither)

  implicit def cogenValidation[A: Cogen, B: Cogen]: Cogen[Validation[A, B]] =
    Cogen[Either[A, B]].contramap(_.toEither)

  implicit val cogenOrdering: Cogen[Ordering] =
    new Cogen[Ordering] {
      def cogen[B](a: Ordering, g: CogenState[B]) = a match {
        case Ordering.GT => variant(0, g)
        case Ordering.EQ => variant(1, g)
        case Ordering.LT => variant(2, g)
      }
    }

  implicit def cogenOneOr[F[_], A: Cogen](implicit F: Cogen[F[A]]): Cogen[OneOr[F, A]] =
    Cogen[F[A] \/ A].contramap(_.run)

  implicit def cogenOneAnd[F[_], A: Cogen](implicit F: Cogen[F[A]]): Cogen[OneAnd[F, A]] =
    Cogen[(A, F[A])].contramap(a => (a.head, a.tail))

  implicit def cogenIList[A](implicit A: Cogen[A]): Cogen[IList[A]] =
    new Cogen[IList[A]] {
      def cogen[B](a: IList[A], g: CogenState[B]) = a match {
        case ICons(h, t) =>
          variant(1, A.cogen(h, cogen(t, g)))
        case INil() =>
          g
      }
    }

  implicit def cogenList[A: Cogen]: Cogen[List[A]] =
    Cogen[IList[A]].contramap(Gen.IListFromList)

  implicit def cogenVector[A: Cogen]: Cogen[Vector[A]] = {
    import std.vector._
    Cogen[IList[A]].contramap(IList.fromFoldable(_))
  }

  implicit def cogenStream[A: Cogen]: Cogen[Stream[A]] = {
    import std.stream._
    Cogen[IList[A]].contramap(IList.fromFoldable(_))
  }

  implicit def cogenArray[A](implicit A: Cogen[A]): Cogen[Array[A]] =
    Cogen[List[A]].contramap(_.toList)

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

  implicit def cogenEphemeralStream[A: Cogen]: Cogen[EphemeralStream[A]] =
    Cogen[List[A]].contramap(_.toList)

  implicit def cogenHeap[A: Cogen]: Cogen[Heap[A]] =
    Cogen[Stream[A]].contramap(_.toUnsortedStream)

  implicit def cogenDequeue[A: Cogen]: Cogen[Dequeue[A]] =
    Cogen[IList[A]].contramap(_.toIList)

  implicit def cogenNonEmptyList[A: Cogen]: Cogen[NonEmptyList[A]] =
    Cogen[(A, List[A])].contramap(nel => (nel.head, nel.tail))

  implicit def cogenIndSeq[A: Cogen]: Cogen[IndSeq[A]] =
    Cogen[List[A]].contramap(Foldable[IndSeq].toList)

  implicit def cogenDiev[A: Cogen]: Cogen[Diev[A]] =
    Cogen[Vector[(A, A)]].contramap(_.intervals)

  implicit val cogenString: Cogen[String] =
    new Cogen[String] {
      def cogen[B](a: String, g: CogenState[B]) =
        cogenIList(cogenChar).cogen(IList(a.toCharArray: _*), g)
    }

  implicit val cogenSymbol: Cogen[Symbol] =
    Cogen[String].contramap(_.name)

  implicit def cogenCoproduct[F[_], G[_], A](implicit A: Cogen[F[A] \/ G[A]]): Cogen[Coproduct[F, G, A]] =
    Cogen[F[A] \/ G[A]].contramap(_.run)

  implicit def cogenConst[A, B](implicit A: Cogen[A]): Cogen[Const[A, B]] =
    A.contramap(_.getConst)

  implicit def cogenZipper[A](implicit A: Cogen[A]): Cogen[Zipper[A]] =
    Cogen[(Stream[A], A, Stream[A])].contramap(z => (z.lefts, z.focus, z.rights))

  implicit def cogenIndexedStoreT[F[_], I: Cogen, A, B](implicit F: Cogen[F[A => B]]): Cogen[IndexedStoreT[F, I, A, B]] =
    Cogen[(F[A => B], I)].contramap(_.run)

  implicit def cogenIndexedContsT[W[_], M[_], R, O, A](implicit F: Cogen[W[A => M[O]] => M[R]]): Cogen[IndexedContsT[W, M, R, O, A]] =
    F.contramap(_.run)

  implicit def cogenEndo[A: Gen: Cogen]: Cogen[Endo[A]] =
    Cogen[A => A].contramap(_.run)

  implicit def cogenEndomorphicKleisliLike[G[_[_], _, _], F[_], A](implicit F: Cogen[G[F, A, A]]): Cogen[Endomorphic[({type l[a, b] = G[F, a, b]})#l, A]] =
    F.contramap(_.run)

  implicit def cogenKleisli[F[_], A, B](implicit F: Cogen[A => F[B]]): Cogen[Kleisli[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenCokleisli[F[_], A, B](implicit F: Cogen[F[A] => B]): Cogen[Cokleisli[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenNullResult[A: Gen, B: Cogen]: Cogen[NullResult[A, B]] =
    Cogen[A => Option[B]].contramap(_.apply)

  implicit def cogenNullArgument[A: Gen, B: Cogen]: Cogen[NullArgument[A, B]] =
    Cogen[Option[A] => B].contramap(_.apply)

  implicit def cogenContravariantCoyoneda[F[_]: Contravariant, A](implicit F: Cogen[F[A]]): Cogen[ContravariantCoyoneda[F, A]] =
    Cogen[F[A]].contramap(_.run)

  implicit def cogenEitherT[F[_], A, B](implicit F: Cogen[F[A \/ B]]): Cogen[EitherT[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenLazyEitherT[F[_], A, B](implicit F: Cogen[F[LazyEither[A, B]]]): Cogen[LazyEitherT[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenMaybeT[F[_], A](implicit F: Cogen[F[Maybe[A]]]): Cogen[MaybeT[F, A]] =
    F.contramap(_.run)

  implicit def cogenStreamT[F[_]: Monad, A](implicit F: Cogen[F[Stream[A]]]): Cogen[StreamT[F, A]] =
    F.contramap(_.toStream)

  implicit def cogenOptionT[F[_], A](implicit F: Cogen[F[Option[A]]]): Cogen[OptionT[F, A]] =
    F.contramap(_.run)

  implicit def cogenLazyOptionT[F[_], A](implicit F: Cogen[F[LazyOption[A]]]): Cogen[LazyOptionT[F, A]] =
    F.contramap(_.run)

  implicit def cogenIdT[F[_], A](implicit F: Cogen[F[A]]): Cogen[IdT[F, A]] =
    F.contramap(_.run)

  implicit def cogenIndexedReaderWriterStateT[F[_], R, W, S1, S2, A](implicit F: Cogen[(R, S1) => F[(W, A, S2)]]): Cogen[IndexedReaderWriterStateT[F, R, W, S1, S2, A]] =
    F.contramap(_.run)

  implicit def cogenIndexedStateT[F[_], S1, S2, A](implicit F: Cogen[S1 => F[(S2, A)]]): Cogen[IndexedStateT[F, S1, S2, A]] =
    F.contramap(s => s.apply(_))

  implicit def cogenWriterT[F[_], A, B](implicit F: Cogen[F[(A, B)]]): Cogen[WriterT[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenUnwriterT[F[_], A, B](implicit F: Cogen[F[(A, B)]]): Cogen[UnwriterT[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenTree[A: Cogen]: Cogen[Tree[A]] =
    new Cogen[Tree[A]] {
      def cogen[B](a: Tree[A], g: CogenState[B]) =
        Cogen[(A, Stream[Tree[A]])].cogen((a.rootLabel, a.subForest), g)
    }

  implicit def cogenTreeLoc[A: Cogen]: Cogen[TreeLoc[A]] =
    Cogen.from4(TreeLoc.unapply)

  implicit def cogenCoyoneda[F[_]: Functor, A](implicit F: Cogen[F[A]]): Cogen[Coyoneda[F, A]] =
    F.contramap(_.run)

  implicit def cogenFuture[A](implicit F: Cogen[A]): Cogen[scala.concurrent.Future[A]] = {
    import scala.concurrent.duration._
    F.contramap(f => Await.result(f, 5.seconds))
  }

  private[this] def nameToValue[A]: Name[A] => A = _.value

  implicit def cogenNeed[A](implicit A: Cogen[A]): Cogen[Need[A]] =
    A.contramap(nameToValue)

  implicit def cogenValue[A](implicit A: Cogen[A]): Cogen[Value[A]] =
    A.contramap(nameToValue)

  implicit def cogenName[A](implicit A: Cogen[A]): Cogen[Name[A]] =
    A.contramap(nameToValue)

  implicit def cogenImmutableArray[A: Cogen]: Cogen[ImmutableArray[A]] =
    Cogen[List[A]].contramap(_.toList)

  implicit def cogenEither3[A1: Cogen, A2: Cogen, A3: Cogen]: Cogen[Either3[A1, A2, A3]] =
    Cogen[A1 \/ A2 \/ A3].contramap{
      case Left3(a) =>
        -\/(-\/(a))
      case Middle3(a) =>
        -\/(\/-(a))
      case Right3(a) =>
        \/-(a)
    }

  implicit def cogenLazyTuple2[A1, A2](implicit A1: Cogen[A1], A2: Cogen[A2]): Cogen[LazyTuple2[A1, A2]] =
    new Cogen[LazyTuple2[A1, A2]] {
      def cogen[B](t: LazyTuple2[A1, A2], g: CogenState[B]) =
        A1.cogen(t._1, A2.cogen(t._2, g))
    }

  implicit def cogenLazyTuple3[A1, A2, A3](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3]): Cogen[LazyTuple3[A1, A2, A3]] =
    new Cogen[LazyTuple3[A1, A2, A3]] {
      def cogen[B](t: LazyTuple3[A1, A2, A3], g: CogenState[B]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, g)))
    }

  implicit def cogenLazyTuple4[A1, A2, A3, A4](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4]): Cogen[LazyTuple4[A1, A2, A3, A4]] =
    new Cogen[LazyTuple4[A1, A2, A3, A4]] {
      def cogen[B](t: LazyTuple4[A1, A2, A3, A4], g: CogenState[B]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, A4.cogen(t._4, g))))
    }

  implicit def cogenPartialFunction[A: Gen, B: Cogen]: Cogen[PartialFunction[A, B]] =
    Cogen[A => Option[B]].contramap(_.lift)

  implicit def cogenBijectionT[F[_], G[_], A, B](implicit A: Gen[A], B: Gen[B], F: Cogen[F[B]], G: Cogen[G[A]]): Cogen[BijectionT[F, G, A, B]] =
    Cogen[(A => F[B], B => G[A])].contramap(b => (b.toK.run, b.fromK.run))

  implicit val cogenNumChar: Cogen[Char @@ GenTags.Num] =
    Tag.subst(Cogen[Char])

  implicit val cogenNumString: Cogen[String @@ GenTags.Num] =
    Tag.subst(Cogen[String])

  implicit val cogenAlphaUpperChar: Cogen[Char @@ GenTags.AlphaUpper] =
    Tag.subst(Cogen[Char])

  implicit val cogenAlphaUpperString: Cogen[String @@ GenTags.AlphaUpper] =
    Tag.subst(Cogen[String])

  implicit val cogenAlphaLowerChar: Cogen[Char @@ GenTags.AlphaLower] =
    Tag.subst(Cogen[Char])

  implicit val cogenAlphaLowerString: Cogen[String @@ GenTags.AlphaLower] =
    Tag.subst(Cogen[String])

  implicit val cogenAlphaChar: Cogen[Char @@ GenTags.Alpha] =
    Tag.subst(Cogen[Char])

  implicit val cogenAlphaString: Cogen[String @@ GenTags.Alpha] =
    Tag.subst(Cogen[String])

  implicit val cogenAlphaNumChar: Cogen[Char @@ GenTags.AlphaNum] =
    Tag.subst(Cogen[Char])

  implicit val cogenAlphaNumString: Cogen[String @@ GenTags.AlphaNum] =
    Tag.subst(Cogen[String])

  implicit val cogenAsciiChar: Cogen[Char @@ GenTags.Ascii] =
    Tag.subst(Cogen[Char])

  implicit val cogenAsciiString: Cogen[String @@ GenTags.Ascii] =
    Tag.subst(Cogen[String])

  implicit def cogenJavaEnum[A <: java.lang.Enum[A]]: Cogen[A] =
    Cogen[Int].contramap(_.ordinal)

  implicit val instance: Divisible[Cogen] =
    new Divisible[Cogen] {
      private[this] val empty = new Cogen[Any] {
        def cogen[X](a: Any, g: CogenState[X]) = g
      }
      def conquer[A] = empty.asInstanceOf[Cogen[A]]
      def contramap[A, B](r: Cogen[A])(f: B => A) =
        r contramap f
      def divide[A, B, C](fa: Cogen[A], fb: Cogen[B])(f: C => (A, B)) =
        new Cogen[C] {
          def cogen[X](c: C, g: CogenState[X]) = {
            val t = f(c)
            fa.cogen(t._1, fb.cogen(t._2, g))
          }
        }
    }

  def apply[A](implicit A: Cogen[A]): Cogen[A] = A
}
