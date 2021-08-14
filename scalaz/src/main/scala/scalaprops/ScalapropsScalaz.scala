package scalaprops

import scalaz._
import scalaz.Isomorphism._
import Gen.gen
import Variant.variantInt

sealed abstract class ScalapropsScalaz0 extends ScalapropsScalaz1 {
  implicit final def cogenEndomorphic[F[_, _], A](implicit F: Cogen[F[A, A]]): Cogen[Endomorphic[F, A]] =
    F.contramap(_.run)

  implicit def cogenIList[A](implicit A: Cogen[A]): Cogen[IList[A]] =
    new Cogen[IList[A]] {
      def cogen[B](a: IList[A], g: CogenState[B]) =
        a match {
          case ICons(h, t) =>
            variantInt(1, A.cogen(h, cogen(t, g)))
          case INil() =>
            g
        }
    }

  implicit final def endomorphicGen[F[_, _], A](implicit F: Gen[F[A, A]]): Gen[Endomorphic[F, A]] =
    F.map(Endomorphic.apply)
}

object ScalapropsScalaz extends ScalapropsScalaz0 {
  private[this] def _nameToValue: Name[Any] => Any = _.value
  private[this] def nameToValue[A]: Name[A] => A = _nameToValue.asInstanceOf[Name[A] => A]

  implicit val randGen: Gen[Rand] =
    Gen.gen { (_, r) =>
      val next = r.next
      (next, next)
    }

  implicit val randCogen: Cogen[Rand] =
    new Cogen[Rand] {
      def cogen[B](a: Rand, g: CogenState[B]) =
        CogenState(g.rand.next, Gen.gen((size, _) => g.gen.f(size, a)))
    }

  implicit val randEqual: Equal[Rand] =
    Equal.equalA[Rand]

  implicit val asPropertyInstance: Contravariant[AsProperty] =
    new Contravariant[AsProperty] {
      def contramap[A, B](r: AsProperty[A])(f: B => A) =
        r contramap f
    }

  implicit val shrinkInstance: InvariantFunctor[Shrink] =
    new InvariantFunctor[Shrink] {
      def xmap[A, B](ma: Shrink[A], f: A => B, g: B => A) =
        ma.xmap(f, g)
    }

  val shrinkFunctionIso: Shrink <~> ({ type l[a] = a => Stream[a] })#l =
    new IsoFunctorTemplate[Shrink, ({ type l[a] = a => Stream[a] })#l] {
      def to_[A](fa: Shrink[A]) = fa.f
      def from_[A](ga: A => Stream[A]) = new Shrink(ga)
    }

  implicit val chooseInstance: InvariantFunctor[Choose] =
    new InvariantFunctor[Choose] {
      def xmap[A, B](ma: Choose[A], f: A => B, g: B => A) =
        new Choose[B] {
          override def withBoundaries(from: B, to: B) =
            ma.withBoundaries(g(from), g(to)).map(f)
          override def choose(from: B, to: B) =
            ma.choose(g(from), g(to)).map(f)
        }
    }

  private[this] val byteIListToByteArray: IList[Byte] => Array[Byte] = { list =>
    val array = new Array[Byte](list.length)
    @annotation.tailrec
    def loop(i: Int, xs: IList[Byte]): Unit =
      xs match {
        case ICons(h, t) =>
          array(i) = h
          loop(i + 1, t)
        case _ =>
      }
    loop(0, list)
    array
  }

  implicit val cogenByteIList: Cogen[IList[Byte]] =
    Cogen[Array[Byte]].contramap(byteIListToByteArray)

  implicit def cogenAlter[F[_], A](implicit F: Cogen[F[A]]): Cogen[Alter[F, A]] =
    F.contramap(_.f)

  implicit def cogenAp[F[_], A](implicit F: Cogen[F[A]]): Cogen[Ap[F, A]] =
    F.contramap(_.f)

  implicit def cogenMaybe[A: Cogen]: Cogen[Maybe[A]] =
    Cogen[Option[A]].contramap(_.toOption)

  implicit def cogenLazyOption[A: Cogen]: Cogen[LazyOption[A]] =
    Cogen[Option[A]].contramap(_.toOption)

  implicit def cogenThese[A, B](implicit A: Cogen[A], B: Cogen[B]): Cogen[A \&/ B] =
    Cogen[(A \/ B) \/ (A, B)].contramap {
      case \&/.Both(a, b) =>
        \/-((a, b))
      case \&/.This(a) =>
        -\/(-\/(a))
      case \&/.That(b) =>
        -\/(\/-(b))
    }

  implicit def cogenLazyEither[A: Cogen, B: Cogen]: Cogen[LazyEither[A, B]] =
    Cogen[Either[A, B]].contramap(_.toEither)

  implicit def cogenDisjunction[A, B](implicit A: Cogen[A], B: Cogen[B]): Cogen[A \/ B] =
    Cogen[Either[A, B]].contramap(_.toEither)

  implicit def cogenValidation[A: Cogen, B: Cogen]: Cogen[Validation[A, B]] =
    Cogen[Either[A, B]].contramap(_.toEither)

  implicit val cogenOrdering: Cogen[Ordering] =
    new Cogen[Ordering] {
      def cogen[B](a: Ordering, g: CogenState[B]) =
        a match {
          case Ordering.GT => variantInt(0, g)
          case Ordering.EQ => variantInt(1, g)
          case Ordering.LT => variantInt(2, g)
        }
    }

  implicit def cogenOneOr[F[_], A: Cogen](implicit F: Cogen[F[A]]): Cogen[OneOr[F, A]] =
    Cogen[F[A] \/ A].contramap(_.run)

  implicit def cogenOneAnd[F[_], A: Cogen](implicit F: Cogen[F[A]]): Cogen[OneAnd[F, A]] =
    Cogen[(A, F[A])].contramap(a => (a.head, a.tail))

  implicit def cogenISet[A: Cogen]: Cogen[ISet[A]] =
    Cogen[List[A]].contramap(_.toList)

  implicit def cogenIMap[A: Cogen, B: Cogen]: Cogen[A ==>> B] =
    Cogen[List[(A, B)]].contramap(_.toList)

  implicit def cogenDList[A: Cogen]: Cogen[DList[A]] =
    Cogen[IList[A]].contramap(_.toIList)

  implicit def cogenEphemeralStream[A: Cogen]: Cogen[EphemeralStream[A]] =
    Cogen[List[A]].contramap(_.toList)

  implicit def cogenCorecursiveList[A: Cogen]: Cogen[CorecursiveList[A]] =
    Cogen[LazyList[A]].contramap(CorecursiveList.lazyListIso.from.apply _)

  implicit def cogenHeap[A: Cogen]: Cogen[Heap[A]] =
    Cogen[EphemeralStream[A]].contramap(_.toUnsortedStream)

  implicit def cogenDequeue[A: Cogen]: Cogen[Dequeue[A]] =
    Cogen[IList[A]].contramap(_.toIList)

  implicit def cogenNonEmptyList[A: Cogen]: Cogen[NonEmptyList[A]] =
    Cogen[(A, IList[A])].contramap(nel => (nel.head, nel.tail))

  implicit def cogenIndSeq[A: Cogen]: Cogen[IndSeq[A]] =
    Cogen[List[A]].contramap(Foldable[IndSeq].toList)

  implicit def cogenDiev[A: Cogen]: Cogen[Diev[A]] =
    Cogen[Vector[(A, A)]].contramap(_.intervals)

  implicit def cogenCoproduct[F[_], G[_], A](implicit A: Cogen[F[A] \/ G[A]]): Cogen[Coproduct[F, G, A]] =
    Cogen[F[A] \/ G[A]].contramap(_.run)

  implicit def cogenConst[A, B](implicit A: Cogen[A]): Cogen[Const[A, B]] =
    A.contramap(_.getConst)

  implicit def cogenZipper[A](implicit A: Cogen[A]): Cogen[Zipper[A]] =
    Cogen[(LazyList[A], A, LazyList[A])].contramap(z => (z.lefts, z.focus, z.rights))

  implicit def cogenTracedT[W[_], A, B](implicit W: Cogen[W[A => B]]): Cogen[TracedT[W, A, B]] =
    W.contramap(_.run)

  implicit def cogenIndexedStoreT[F[_], I: Cogen, A, B](implicit
    F: Cogen[F[A => B]]
  ): Cogen[IndexedStoreT[F, I, A, B]] =
    Cogen[(F[A => B], I)].contramap(_.run)

  implicit def cogenIndexedContsT[W[_], M[_], R, O, A](implicit
    F: Cogen[W[A => M[O]] => M[R]]
  ): Cogen[IndexedContsT[W, R, O, M, A]] =
    F.contramap(_.run)

  implicit def cogenSelectT[R, M[_], A](implicit
    F: Cogen[(A => M[R]) => M[A]]
  ): Cogen[SelectT[R, M, A]] =
    F.contramap(_.run)

  implicit def cogenEndo[A: Gen: Cogen]: Cogen[scalaz.Endo[A]] =
    Cogen[A => A].contramap(_.run)

  implicit def cogenEndomorphicKleisliLike[G[_[_], _, _], F[_], A](implicit
    F: Cogen[G[F, A, A]]
  ): Cogen[Endomorphic[({ type l[a, b] = G[F, a, b] })#l, A]] =
    F.contramap(_.run)

  implicit def cogenKleisli[F[_], A, B](implicit F: Cogen[A => F[B]]): Cogen[Kleisli[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenCokleisli[F[_], A, B](implicit F: Cogen[F[A] => B]): Cogen[Cokleisli[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenNullResult[A: Gen, B: Cogen]: Cogen[NullResult[A, B]] =
    Cogen[A => Option[B]].contramap(_.apply)

  implicit def cogenNullArgument[A: Gen, B: Cogen]: Cogen[NullArgument[A, B]] =
    Cogen[Option[A] => B].contramap(_.apply)

  implicit def cogenContravariantCoyoneda[F[_]: Contravariant, A](implicit
    F: Cogen[F[A]]
  ): Cogen[ContravariantCoyoneda[F, A]] =
    Cogen[F[A]].contramap(_.run)

  implicit def cogenEitherT[F[_], A, B](implicit F: Cogen[F[A \/ B]]): Cogen[EitherT[A, F, B]] =
    F.contramap(_.run)

  implicit def cogenLazyEitherT[F[_], A, B](implicit F: Cogen[F[LazyEither[A, B]]]): Cogen[LazyEitherT[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenTheseT[F[_], A, B](implicit F: Cogen[F[A \&/ B]]): Cogen[TheseT[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenMaybeT[F[_], A](implicit F: Cogen[F[Maybe[A]]]): Cogen[MaybeT[F, A]] =
    F.contramap(_.run)

  implicit def cogenStreamT[F[_]: Monad, A](implicit F: Cogen[F[LazyList[A]]]): Cogen[StreamT[F, A]] =
    F.contramap(_.toLazyList)

  implicit def cogenOptionT[F[_], A](implicit F: Cogen[F[Option[A]]]): Cogen[OptionT[F, A]] =
    F.contramap(_.run)

  implicit def cogenLazyOptionT[F[_], A](implicit F: Cogen[F[LazyOption[A]]]): Cogen[LazyOptionT[F, A]] =
    F.contramap(_.run)

  implicit def cogenIdT[F[_], A](implicit F: Cogen[F[A]]): Cogen[IdT[F, A]] =
    F.contramap(_.run)

  implicit def cogenIndexedReaderWriterStateT[F[_]: Monad, R, W, S1, S2, A](implicit
    F: Cogen[(R, S1) => F[(W, A, S2)]]
  ): Cogen[IndexedReaderWriterStateT[R, W, S1, S2, F, A]] =
    F.contramap(_.run)

  implicit def cogenIndexedStateT[F[_]: Monad, S1, S2, A](implicit
    F: Cogen[S1 => F[(S2, A)]]
  ): Cogen[IndexedStateT[S1, S2, F, A]] =
    F.contramap(s => s.apply(_))

  implicit def cogenWriterT[F[_], A, B](implicit F: Cogen[F[(A, B)]]): Cogen[WriterT[A, F, B]] =
    F.contramap(_.run)

  implicit def cogenUnwriterT[F[_], A, B](implicit F: Cogen[F[(A, B)]]): Cogen[UnwriterT[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenTree[A: Cogen]: Cogen[scalaz.Tree[A]] =
    new Cogen[scalaz.Tree[A]] {
      def cogen[B](a: scalaz.Tree[A], g: CogenState[B]) =
        Cogen[(A, EphemeralStream[scalaz.Tree[A]])].cogen((a.rootLabel, a.subForest), g)
    }

  implicit def cogenTreeLoc[A: Cogen]: Cogen[scalaz.TreeLoc[A]] = {
    import scalaz.TreeLoc._
    Cogen[(Tree[A], TreeForest[A], TreeForest[A], Parents[A])].contramap { a =>
      (a.tree, a.lefts, a.rights, a.parents)
    }
  }

  implicit def cogenStrictTree[A: Cogen]: Cogen[StrictTree[A]] =
    new Cogen[StrictTree[A]] {
      def cogen[B](a: StrictTree[A], g: CogenState[B]) =
        Cogen[(A, Vector[StrictTree[A]])].cogen((a.rootLabel, a.subForest), g)
    }

  implicit def cogenCoyoneda[F[_]: Functor, A](implicit F: Cogen[F[A]]): Cogen[Coyoneda[F, A]] =
    F.contramap(_.run)

  implicit def cogenNeed[A](implicit A: Cogen[A]): Cogen[scalaz.Need[A]] =
    A.contramap(nameToValue)

  implicit def cogenValue[A](implicit A: Cogen[A]): Cogen[Value[A]] =
    A.contramap(nameToValue)

  implicit def cogenName[A](implicit A: Cogen[A]): Cogen[Name[A]] =
    A.contramap(nameToValue)

  implicit def cogenImmutableArray[A: Cogen]: Cogen[ImmutableArray[A]] =
    Cogen[List[A]].contramap(_.toList)

  implicit def cogenEither3[A1: Cogen, A2: Cogen, A3: Cogen]: Cogen[Either3[A1, A2, A3]] =
    Cogen[A1 \/ A2 \/ A3].contramap {
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

  implicit def cogenLazyTuple3[A1, A2, A3](implicit
    A1: Cogen[A1],
    A2: Cogen[A2],
    A3: Cogen[A3]
  ): Cogen[LazyTuple3[A1, A2, A3]] =
    new Cogen[LazyTuple3[A1, A2, A3]] {
      def cogen[B](t: LazyTuple3[A1, A2, A3], g: CogenState[B]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, g)))
    }

  implicit def cogenLazyTuple4[A1, A2, A3, A4](implicit
    A1: Cogen[A1],
    A2: Cogen[A2],
    A3: Cogen[A3],
    A4: Cogen[A4]
  ): Cogen[LazyTuple4[A1, A2, A3, A4]] =
    new Cogen[LazyTuple4[A1, A2, A3, A4]] {
      def cogen[B](t: LazyTuple4[A1, A2, A3, A4], g: CogenState[B]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, A4.cogen(t._4, g))))
    }

  implicit def cogenBijectionT[F[_], G[_], A, B](implicit
    A: Gen[A],
    B: Gen[B],
    F: Cogen[F[B]],
    G: Cogen[G[A]]
  ): Cogen[BijectionT[F, G, A, B]] =
    Cogen[(A => F[B], B => G[A])].contramap(b => (b.toK.run, b.fromK.run))

  implicit def cogenTannen[F[_], G[_, _], A, B](implicit F: Cogen[F[G[A, B]]]): Cogen[Tannen[F, G, A, B]] =
    F.contramap(_.f)

  implicit val cogenInstance: Divisible[Cogen] =
    new Divisible[Cogen] {
      override def conquer[A] =
        Cogen.conquer[A]
      override def contramap[A, B](r: Cogen[A])(f: B => A) =
        r contramap f
      override def divide2[A, B, C](fa: => Cogen[A], fb: => Cogen[B])(f: C => (A, B)) =
        Cogen.divide(fa, fb)(f)
    }

  implicit val cogenStateInstance: Functor[CogenState] =
    new Functor[CogenState] {
      override def map[A, B](fa: CogenState[A])(f: A => B) =
        fa map f
    }

  val isoReaderState: Gen <~> ({ type x[a] = Kleisli[({ type y[b] = scalaz.State[Rand, b] })#y, Int, a] })#x =
    new IsoFunctorTemplate[Gen, ({ type x[a] = Kleisli[({ type y[b] = scalaz.State[Rand, b] })#y, Int, a] })#x] {
      override def to_[A](fa: Gen[A]) =
        Kleisli[({ type l[a] = scalaz.State[Rand, a] })#l, Int, A] { size => scalaz.State { rand => fa.f(size, rand) } }
      override def from_[A](ga: Kleisli[({ type l[a] = scalaz.State[Rand, a] })#l, Int, A]) =
        gen((size, rand) => ga.run(size).run(rand))
    }

  val isoStateReader: Gen <~> ({ type x[a] = StateT[Rand, ({ type y[b] = Reader[Int, b] })#y, a] })#x =
    new IsoFunctorTemplate[Gen, ({ type x[a] = StateT[Rand, ({ type y[b] = Reader[Int, b] })#y, a] })#x] {
      override def to_[A](fa: Gen[A]) =
        StateT[Rand, ({ type l[a] = Reader[Int, a] })#l, A] { rand => Reader { size => fa.f(size, rand) } }
      override def from_[A](ga: StateT[Rand, ({ type y[b] = Reader[Int, b] })#y, A]) =
        gen((size, rand) => ga.run(rand).run(size))
    }

  val isoRWS: Gen <~> ({ type l[a] = RWS[Int, Unit, Rand, a] })#l =
    new IsoFunctorTemplate[Gen, ({ type l[a] = RWS[Int, Unit, Rand, a] })#l] {
      override def to_[A](fa: Gen[A]) =
        RWS { (size, rand) =>
          val a = fa.f(size, rand)
          ((), a._2, a._1)
        }
      override def from_[A](ga: RWS[Int, Unit, Rand, A]) =
        Gen.gen { (size, rand) =>
          val a = ga.run(size, rand)
          (a._3, a._2)
        }
    }

  val isoFunction: Gen <~> ({ type l[a] = (Int, Rand) => (Rand, a) })#l =
    new IsoFunctorTemplate[Gen, ({ type l[a] = (Int, Rand) => (Rand, a) })#l] {
      override def to_[A](fa: Gen[A]) = fa.f
      override def from_[A](ga: (Int, Rand) => (Rand, A)) = Gen.gen(ga)
    }

  implicit def ilistGen[A](implicit A: Gen[A]): Gen[IList[A]] = {
    import scalaz.std.list._
    Gen.listOf(A).map(IList.fromFoldable(_)) // TODO optimize
  }

  implicit val genInstance: Monad[Gen] with BindRec[Gen] =
    new Monad[Gen] with BindRec[Gen] {
      override def bind[A, B](fa: Gen[A])(f: A => Gen[B]) =
        fa flatMap f
      override def map[A, B](fa: Gen[A])(f: A => B) =
        fa map f
      override def point[A](a: => A) =
        Gen.value(a)
      type F[a] = scalaz.State[Rand, a]
      private[this] val F = Kleisli.kleisliBindRec[F, Int]
      override def tailrecM[A, B](a: A)(f: A => Gen[A \/ B]): Gen[B] = {
        val g = f.andThen(isoReaderState.to(_))
        isoReaderState.from(F.tailrecM(a)(g))
      }
    }

  implicit def alterGen[F[_], A](implicit F: Gen[F[A]]): Gen[Alter[F, A]] =
    F.map(Alter(_))

  implicit def apGen[F[_], A](implicit F: Gen[F[A]]): Gen[Ap[F, A]] =
    F.map(Ap(_))

  implicit def maybeGen[A](implicit A: Gen[A]): Gen[Maybe[A]] =
    Gen[Option[A]].map(Maybe.fromOption(_))

  implicit def lazyTuple2Gen[A1, A2](implicit A1: Gen[A1], A2: Gen[A2]): Gen[LazyTuple2[A1, A2]] =
    Apply[Gen].apply2(A1, A2)(LazyTuple2(_, _))

  implicit def lazyTuple3Gen[A1, A2, A3](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3]): Gen[LazyTuple3[A1, A2, A3]] =
    Apply[Gen].apply3(A1, A2, A3)(LazyTuple3(_, _, _))

  implicit def lazyTuple4Gen[A1, A2, A3, A4](implicit
    A1: Gen[A1],
    A2: Gen[A2],
    A3: Gen[A3],
    A4: Gen[A4]
  ): Gen[LazyTuple4[A1, A2, A3, A4]] =
    Apply[Gen].apply4(A1, A2, A3, A4)(LazyTuple4(_, _, _, _))

  implicit def lazyOptionGen[A: Gen]: Gen[LazyOption[A]] =
    Gen[Maybe[A]].map {
      case Maybe.Just(a) => LazyOption.lazySome(a)
      case Maybe.Empty() => LazyOption.lazyNone[A]
    }

  implicit def lazyEitherGen[A1, A2](implicit A1: Gen[A1], A2: Gen[A2]): Gen[LazyEither[A1, A2]] =
    Gen.oneOf(
      A1.map(LazyEither.lazyLeft[A2].apply(_)),
      A2.map(LazyEither.lazyRight[A1].apply(_))
    )

  implicit def imapGen[A: Order: Gen, B: Gen]: Gen[A ==>> B] =
    Gen[List[(A, B)]].map(==>>.fromList(_))

  implicit def isetGen[A: Order: Gen]: Gen[ISet[A]] =
    Gen[List[A]].map(ISet.fromList(_))

  implicit def kleisliGen[F[_], A, B](implicit F: Gen[A => F[B]]): Gen[Kleisli[F, A, B]] =
    F.map(Kleisli(_))

  implicit def cokleisliGen[F[_], A, B](implicit F: Gen[F[A] => B]): Gen[Cokleisli[F, A, B]] =
    F.map(Cokleisli(_))

  implicit def nullResultGen[A, B](implicit F: Gen[A => Option[B]]): Gen[NullResult[A, B]] =
    F.map(NullResult(_))

  implicit def nullArgumentGen[A, B](implicit F: Gen[Option[A] => B]): Gen[NullArgument[A, B]] =
    F.map(NullArgument(_))

  implicit def nonEmptyListGen[A](implicit A: Gen[A]): Gen[NonEmptyList[A]] =
    Apply[Gen].apply2(A, Gen[IList[A]])((x, xs) => NonEmptyList.nel(x, xs.drop(1)))

  implicit def oneAndGen[F[_], A](implicit F: Gen[F[A]], A: Gen[A]): Gen[OneAnd[F, A]] =
    Apply[Gen].apply2(A, F)(OneAnd(_, _))

  implicit def oneOrGen[F[_], A](implicit F: Gen[F[A]], A: Gen[A]): Gen[OneOr[F, A]] =
    Gen[F[A] \/ A].map(OneOr.apply)

  implicit def dequeueGen[A](implicit A: Gen[A]): Gen[Dequeue[A]] =
    Gen.oneOf(
      Gen.value(Dequeue.empty[A]),
      A.map(Dequeue.apply(_)),
      Apply[Gen].apply2(Gen[NonEmptyList[A]], Gen[NonEmptyList[A]])((x, y) =>
        Dequeue.fromFoldable(x) ++ Dequeue.fromFoldable(y)
      )
    )

  implicit def disjunctionGen[A, B](implicit A: Gen[A], B: Gen[B]): Gen[A \/ B] =
    Gen.oneOf(A.map(\/.left), B.map(\/.right))

  implicit def eitherGen[A, B](implicit A: Gen[A], B: Gen[B]): Gen[A Either B] =
    Gen.oneOf(B.map(Right(_)), A.map(Left(_)))

  implicit def lazyEitherTGen[F[_], A, B](implicit F: Gen[F[LazyEither[A, B]]]): Gen[LazyEitherT[F, A, B]] =
    F.map(LazyEitherT(_))

  implicit def eitherTGen[F[_], A, B](implicit F: Gen[F[A \/ B]]): Gen[EitherT[A, F, B]] =
    F.map(EitherT(_))

  implicit def theseTGen[F[_], A, B](implicit F: Gen[F[A \&/ B]]): Gen[TheseT[F, A, B]] =
    F.map(TheseT.theseT[F, A, B](_))

  implicit def maybeTGen[F[_], A](implicit F: Gen[F[Maybe[A]]]): Gen[MaybeT[F, A]] =
    F.map(MaybeT.apply(_))

  implicit def optionTGen[F[_], A](implicit F: Gen[F[Option[A]]]): Gen[OptionT[F, A]] =
    F.map(OptionT.apply(_))

  implicit def lazyOptionTGen[F[_], A](implicit F: Gen[F[LazyOption[A]]]): Gen[LazyOptionT[F, A]] =
    F.map(LazyOptionT(_))

  implicit def idTGen[F[_], A](implicit F: Gen[F[A]]): Gen[IdT[F, A]] =
    F.map(IdT(_))

  implicit def coproductGen[F[_], G[_], A](implicit F: Gen[F[A] \/ G[A]]): Gen[Coproduct[F, G, A]] =
    F.map(Coproduct(_))

  implicit def constGen[A, B](implicit A: Gen[A]): Gen[Const[A, B]] =
    A.map(Const(_))

  implicit def writerTGen[F[_], A, B](implicit F: Gen[F[(A, B)]]): Gen[WriterT[A, F, B]] =
    F.map(WriterT(_))

  implicit def unwriterTGen[F[_], A, B](implicit F: Gen[F[(A, B)]]): Gen[UnwriterT[F, A, B]] =
    F.map(UnwriterT(_))

  implicit def indexedStateTGen[F[_]: Monad, S1, S2, A](implicit
    F: Gen[S1 => F[(S2, A)]]
  ): Gen[IndexedStateT[S1, S2, F, A]] =
    F.map(IndexedStateT(_))

  implicit def indexedContsTGen[W[_], M[_], R, O, A](implicit
    F: Gen[W[A => M[O]] => M[R]]
  ): Gen[IndexedContsT[W, R, O, M, A]] =
    F.map(IndexedContsT(_))

  implicit def selectTGen[R, M[_], A](implicit
    F: Gen[(A => M[R]) => M[A]]
  ): Gen[SelectT[R, M, A]] =
    F.map(SelectT(_))

  implicit def indexedReaderWriterStateTGen[F[_], R, W, S1, S2, A](implicit
    F: Gen[(R, S1) => F[(W, A, S2)]]
  ): Gen[IndexedReaderWriterStateT[R, W, S1, S2, F, A]] =
    F.map(IndexedReaderWriterStateT.apply)

  implicit def streamTGen[F[_]: Applicative, A](implicit F: Gen[F[LazyList[A]]]): Gen[StreamT[F, A]] =
    F.map(StreamT.fromLazyList(_))

  implicit def theseGen[A, B](implicit A: Gen[A], B: Gen[B]): Gen[A \&/ B] =
    Gen.oneOf(
      A.map(\&/.This.apply),
      B.map(\&/.That.apply),
      Apply[Gen].apply2(A, B)(\&/.Both.apply)
    )

  implicit def listTGen[F[_], A](implicit F: Gen[F[IList[A]]]): Gen[ListT[F, A]] =
    F.map(ListT.apply(_))

  implicit def dlistGen[A: Gen]: Gen[DList[A]] =
    Gen[List[A]].map(DList.fromList(_))

  implicit def heapGen[A: Gen: Order]: Gen[Heap[A]] =
    Gen[IList[A]].map(Heap.fromCodata(_))

  implicit def ephemeralStreamGen[A: Gen]: Gen[EphemeralStream[A]] =
    Gen[Stream[A]].map(EphemeralStream.fromStream(_))

  implicit def corecursiveListGen[A: Gen]: Gen[CorecursiveList[A]] =
    Gen[LazyList[A]].map(CorecursiveList.fromLazyList)

  implicit def monoidCoproduct[A: Gen, B: Gen]: Gen[A :+: B] =
    Gen[Vector[A \/ B]].map(new :+:(_))

  implicit def indexedStoreTGen[F[_], I: Gen, A, B](implicit F: Gen[F[A => B]]): Gen[IndexedStoreT[F, I, A, B]] =
    Gen[(F[A => B], I)].map(IndexedStoreT(_))

  implicit def tracedTGen[W[_], A, B](implicit W: Gen[W[A => B]]): Gen[TracedT[W, A, B]] =
    W.map(TracedT(_))

  implicit val orderingGen: Gen[Ordering] =
    Gen.elements(
      scalaz.Ordering.GT,
      scalaz.Ordering.EQ,
      scalaz.Ordering.LT
    )

  implicit def validationGen[A: Gen, B: Gen]: Gen[Validation[A, B]] =
    Gen[A \/ B].map(_.toValidation)

  implicit def zipperGen[A: Gen]: Gen[Zipper[A]] =
    Gen.sized {
      case n if n <= 1 =>
        Gen[A].map(a => Zipper(LazyList.empty, a, LazyList.empty))
      case n =>
        val z = n - 1
        Gen.choose(0, z).flatMap { rSize =>
          val lSize = z - rSize
          Apply[Gen].apply3(
            Gen.sequenceNList(lSize, Gen[A]).map(_.to(LazyList)),
            Gen[A],
            Gen.sequenceNList(rSize, Gen[A]).map(_.to(LazyList))
          )(Zipper(_, _, _))
        }
    }

  implicit def coyonedaGen[F[_], A](implicit F: Gen[F[A]]): Gen[Coyoneda[F, A]] =
    F.map(Coyoneda.lift)

  implicit def codensityGen[F[_]: Monad, A](implicit F: Gen[F[A]], A: Gen[A]): Gen[Codensity[F, A]] =
    Gen.oneOf(
      F.map(Codensity.rep[F, A]),
      A.map(Codensity.pureCodensity(_))
    )

  implicit def indSeqGen[A: Gen]: Gen[IndSeq[A]] =
    Gen[List[A]].map(IndSeq.fromSeq)

  implicit def dievGen[A: Gen: Enum]: Gen[Diev[A]] =
    Gen[List[A]].map(Diev.fromValuesSeq(_))

  implicit def either3Gen[A1, A2, A3](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3]): Gen[Either3[A1, A2, A3]] =
    Gen.oneOf(
      A1.map(Either3.left3),
      A2.map(Either3.middle3),
      A3.map(Either3.right3)
    )

  implicit def immutableArrayGen[A: Gen: reflect.ClassTag]: Gen[ImmutableArray[A]] =
    Gen[Array[A]].map(ImmutableArray.fromArray)

  implicit def nameGen[A](implicit A: Gen[A]): Gen[Name[A]] =
    A.map(Name.apply(_))

  implicit def valueGen[A](implicit A: Gen[A]): Gen[Value[A]] =
    A.map(Value.apply(_))

  implicit def needGen[A](implicit A: Gen[A]): Gen[scalaz.Need[A]] =
    A.map(scalaz.Need.apply(_))

  implicit def scalazEndoGen[A: Gen: Cogen]: Gen[scalaz.Endo[A]] =
    Gen[A => A].map(scalaz.Endo(_))

  implicit def kleisliLikeEndoGen[G[_[_], _, _], F[_], A](implicit
    F: Gen[G[F, A, A]]
  ): Gen[Endomorphic[({ type l[a, b] = G[F, a, b] })#l, A]] =
    endomorphicGen[({ type l[a, b] = G[F, a, b] })#l, A]

  implicit def contravariantCoyonedaGen[F[_], A](implicit F: Gen[F[A]]): Gen[ContravariantCoyoneda[F, A]] =
    F.map(ContravariantCoyoneda.lift)

  implicit def fingerGen[V: Monoid, A](implicit A: Gen[A], R: Reducer[A, V]): Gen[FingerTree.Finger[V, A]] =
    Gen.oneOf(
      A.map(FingerTree.one[V, A]),
      Apply[Gen].apply2(A, A)(FingerTree.two[V, A]),
      Apply[Gen].apply3(A, A, A)(FingerTree.three[V, A]),
      Apply[Gen].apply4(A, A, A, A)(FingerTree.four[V, A])
    )
  private[scalaprops] def strictTreeGenSized[A: NotNothing](size: Int)(implicit A: Gen[A]): Gen[StrictTree[A]] =
    size match {
      case n if n <= 1 =>
        A.map(a => StrictTree.Leaf(a))
      case 2 =>
        Gen[(A, A)].map { case (a1, a2) =>
          StrictTree.Node(a1, Vector(StrictTree.Leaf(a2)))
        }
      case 3 =>
        Gen[(A, A, A)].flatMap { case (a1, a2, a3) =>
          Gen.elements(
            StrictTree.Node(a1, Vector(StrictTree.Leaf(a2), StrictTree.Leaf(a3))),
            StrictTree.Node(a1, Vector(StrictTree.Node(a2, Vector(StrictTree.Leaf(a3)))))
          )
        }
      case _ =>
        withSize(size - 1)(strictTreeGenSized[A]).flatMap { as => A.map(a => StrictTree.Node(a, as.toVector)) }
    }

  implicit def strictTreeGen[A](implicit A: Gen[A]): Gen[StrictTree[A]] =
    Gen.sized(n => Gen.choose(0, n).flatMap(strictTreeGenSized[A]))
  private[this] def withSize[A](size: Int)(f: Int => Gen[A]): Gen[Stream[A]] = {
    import scalaz.std.stream._
    import scalaz.State
    Applicative[Gen]
      .sequence(
        Stream.fill(size)(Gen.choose(1, size))
      )
      .flatMap { s =>
        val ns = Traverse[Stream]
          .traverseS(s) { n =>
            for {
              sum <- State.get[Int]
              r <-
                if (sum >= size) {
                  State.state[Int, Option[Int]](None)
                } else if ((sum + n) > size) {
                  State((s: Int) => (s + n) -> Option(size - sum))
                } else {
                  State((s: Int) => (s + n) -> Option(n))
                }
            } yield r
          }
          .eval(0)
          .flatten
        Applicative[Gen].sequence(ns.map(f))
      }
  }
  private[scalaprops] def treeGenSized[A: NotNothing](size: Int)(implicit A: Gen[A]): Gen[scalaz.Tree[A]] = {
    import scalaz.Tree
    size match {
      case n if n <= 1 =>
        A.map(a => Tree.Leaf(a))
      case 2 =>
        Gen[(A, A)].map { case (a1, a2) =>
          Tree.Node(a1, EphemeralStream(Tree.Leaf(a2)))
        }
      case 3 =>
        Gen[(A, A, A)].flatMap { case (a1, a2, a3) =>
          Gen.elements(
            Tree.Node(a1, EphemeralStream(Tree.Leaf(a2), Tree.Leaf(a3))),
            Tree.Node(a1, EphemeralStream(Tree.Node(a2, EphemeralStream(Tree.Leaf(a3)))))
          )
        }
      case _ =>
        withSize(size - 1)(treeGenSized[A]).flatMap { as => A.map(a => Tree.Node(a, EphemeralStream.fromStream(as))) }
    }
  }

  implicit def treeGen[A](implicit A: Gen[A]): Gen[scalaz.Tree[A]] = {
    Gen.sized(n => Gen.choose(0, n).flatMap(treeGenSized[A]))
  }

  private[scalaprops] def treeLocGenSized[A](size: Int)(implicit A: Gen[A]): Gen[scalaz.TreeLoc[A]] = {
    import scalaz.TreeLoc
    def forest(n: Int): Gen[TreeLoc.TreeForest[A]] =
      withSize(n)(treeGenSized[A]).map(EphemeralStream.fromStream(_))

    val parent: Int => Gen[TreeLoc.Parent[A]] = { n =>
      Gen.choose(0, n - 1).flatMap { x1 =>
        Apply[Gen].tuple3(
          forest(x1),
          A,
          forest(n - x1 - 1)
        )
      }
    }

    for {
      a <- Gen.choose(1, size)
      b = size - a
      aa <- Gen.choose(1, a)
      ba <- Gen.choose(0, b)
      t <- Apply[Gen].apply4(
        treeGenSized[A](aa),
        forest(a - aa),
        forest(ba),
        withSize(b - ba)(parent).map(EphemeralStream.fromStream(_))
      )(TreeLoc.apply[A])
    } yield t
  }

  implicit def treeLocGen[A: Gen]: Gen[scalaz.TreeLoc[A]] =
    Gen.sized(treeLocGenSized(_))

  implicit def bijectionTGen[F[_], G[_], A, B](implicit
    A: Cogen[A],
    B: Cogen[B],
    F: Gen[F[B]],
    G: Gen[G[A]]
  ): Gen[BijectionT[F, G, A, B]] =
    Gen[(A => F[B], B => G[A])].map { case (f, g) => BijectionT.bijection(f, g) }

  implicit def freeTGen[S[_]: Functor, M[_]: Applicative, A: Gen](implicit
    G1: Gen[S[A]],
    G2: Gen[M[A]]
  ): Gen[FreeT[S, M, A]] =
    Gen.oneOf(
      Gen[A].map(FreeT.point[S, M, A](_)),
      G2.map(FreeT.liftM[S, M, A](_)),
      G1.map(FreeT.liftF[S, M, A](_))
    )

  implicit def tannenGen[F[_], G[_, _], A, B](implicit F: Gen[F[G[A, B]]]): Gen[Tannen[F, G, A, B]] =
    F.map(Tannen[F, G, A, B](_))
}
