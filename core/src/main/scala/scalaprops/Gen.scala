package scalaprops

import Gen.gen
import scala.collection.generic.CanBuildFrom
import scala.concurrent.Future
import scalaz._

final case class Gen[A] private(f: (Int, Rand) => (A, Rand)) {

  def map[B](g: A => B): Gen[B] =
    gen{ (i, r) =>
      val (a, r0) = f(i, r)
      (g(a), r0)
    }

  def flatMap[B](g: A => Gen[B]): Gen[B] =
    gen{ (i, r) =>
      val (a, r0) = f(i, r)
      g(a).f(i, r0)
    }

  def resize(s: Int): Gen[A] =
    mapSize(Function.const(s))

  def mapSize(g: Int => Int): Gen[A] =
    gen((s, r) => f(g(s), r))

  /** convenience method for get sample values */
  def samples(size: Int = 100, seed: Int = Rand.defaultSeed): List[A] =
    Gen.sequenceNList(size, this).f(size, Rand.fromSeed(seed))._1
}


sealed abstract class GenInstances0 extends GenInstances {

  implicit final def endomorphicGen[F[_, _], A](implicit F: Gen[F[A, A]]): Gen[Endomorphic[F, A]] =
    F.map(Endomorphic.apply)

}

object Gen extends GenInstances0 {
  def gen[A](f: (Int, Rand) => (A, Rand)): Gen[A] =
    new Gen(f)

  def apply[A](implicit A: Gen[A]): Gen[A] = A

  def value[A](a: A): Gen[A] =
    gen((_, r) => (a, r))

  def oneOf[A](x: Gen[A], xs: Gen[A]*): Gen[A] = {
    val array = (x +: xs).toArray[Any]
    choose(0, xs.length).flatMap(array(_).asInstanceOf[Gen[A]])
  }

  def oneOfLazy[A](x: Need[Gen[A]], xs: Need[Gen[A]]*): Gen[A] = {
    val array = (x +: xs).toArray[Need[Any]]
    choose(0, xs.length).flatMap(array(_).asInstanceOf[Need[Gen[A]]].value)
  }

  private[this] def sequenceN[F[_]: Traverse, A](n: Int, g: Gen[A], f: List[A] => F[A]): Gen[F[A]] =
    sequenceNList(n, g).map(f)

  private[this] def sequenceNIList[F[_], A](n: Int, g: Gen[A]): Gen[IList[A]] =
    sequenceN[IList, A](n, g, IList.fromList)

  def sequenceNList[F[_], A](n: Int, g: Gen[A]): Gen[List[A]] = {
    @annotation.tailrec
    def loop(size: Int, i: Int, next: Rand, acc: List[A]): (List[A], Rand) = {
      if (i < n) {
        val r = g.f(size, next)
        loop(size, i + 1, r._2, r._1 :: acc)
      } else {
        (acc.reverse, next)
      }
    }
    gen{ (size, r) =>
      loop(size, 0, r, List.empty[A])
    }
  }

  def parameterised[A](f: (Int, Rand) => Gen[A]): Gen[A] =
    gen((i, r) => f(i, r).f(i, r))

  def sized[A](f: Int => Gen[A]): Gen[A] =
    parameterised((i, _) => f(i))

  def choose(from: Int, to: Int): Gen[Int] =
    gen{ (_, r) => r.choose(from, to) }

  def chooseR(from: Int, to: Int, r: Rand): Gen[Int] =
    gen{ (_, _) =>
      r.choose(from, to)
    }

  @annotation.tailrec
  private[this] def pick0[B](n: Int, gs: IList[(Int, Gen[B])]): Gen[B] = gs match {
    case INil() => sys.error(s"bug? $n $gs")
    case ICons(h, t) =>
      val k = h._1
      if (n <= k) h._2
      else pick0(n - k, t)
  }

  def frequency[A](gs: NonEmptyList[(Int, Gen[A])]): Gen[A] = {
    import std.anyVal._
    val F = Foldable[NonEmptyList]
    choose(1, F.foldMap(gs)(_._1)).flatMap{ i =>
      pick0(i, F.toIList(gs))
    }
  }

  def frequency[A](g: (Int, Gen[A]), gs: (Int, Gen[A]) *): Gen[A] =
    frequency(NonEmptyList.nels(g, gs: _*))

  def elemFrequency[A](as: NonEmptyList[(Int, A)]): Gen[A] =
    frequency(as.map{case (i, a) => i -> Gen.value(a)})

  def elements[A](a: A, as: A*): Gen[A] = {
    val xs = (a +: as).toArray[Any]
    choose(0, as.length).map{xs(_).asInstanceOf[A]}
  }

  private[this] def listOf_[F[_]: Traverse, A](g: Gen[A], x: Int, f: List[A] => F[A]): Gen[F[A]] =
    parameterised{ (size, r) =>
      chooseR(x, size, r).flatMap{ n =>
        sequenceN(n, g, f)
      }
    }

  private[this] def listOfCBF[F[_]: Traverse, A](g: Gen[A], x: Int)(implicit F: CanBuildFrom[Nothing, A, F[A]]): Gen[F[A]] =
    listOf_[F, A](g, x, _.to[F])

  private[this] def listOfCBF0[F[_]: Traverse, A](g: Gen[A])(implicit F: CanBuildFrom[Nothing, A, F[A]]): Gen[F[A]] =
    listOfCBF[F, A](g, 0)

  def listOf[A](g: Gen[A], x: Int = 0): Gen[IList[A]] =
    listOf_[IList, A](g, x, IList.fromList)

  implicit def ilist[A](implicit A: Gen[A]): Gen[IList[A]] =
    listOf(A)

  implicit def vector[A](implicit A: Gen[A]): Gen[Vector[A]] = {
    import scalaz.std.vector._
    listOfCBF0[Vector, A](A)
  }

  implicit def mapGen[A: Gen, B: Gen]: Gen[Map[A, B]] =
    list[(A, B)].map(_.toMap)

  implicit def setGen[A: Gen]: Gen[Set[A]] =
    list[A].map(_.toSet)

  implicit def streamGen[A](implicit A: Gen[A]): Gen[Stream[A]] = {
    import scalaz.std.stream._
    listOfCBF0[Stream, A](A)
  }

  implicit def list[A](implicit A: Gen[A]): Gen[List[A]] = {
    import scalaz.std.list._
    listOfCBF0[List, A](A)
  }

  implicit def arrayGen[A: reflect.ClassTag: Gen]: Gen[Array[A]] =
    Gen[List[A]].map(_.toArray)

  def listOf1[A](g: Gen[A]): Gen[IList[A]] =
    listOf(g, 1)

  private[this] def pick[A](n: Int, as: IList[A]): Gen[IList[A]] = {
    val len = as.length
    if(n < 0 || n > len) {
      sys.error(s"bug $n $as")
    } else {
      sequenceNIList(n, choose(0, len - 1)).map{ is =>
        @annotation.tailrec
        def loop(iis: IList[Int], aas: IList[(A, Int)], acc: IList[A]): IList[A] =
          (iis, aas) match {
            case (ICons(h1, t1), ICons(h2, t2)) =>
              if(h1 == h2._2) loop(t1, t2, acc)
              else loop(iis, t2, h2._1 :: acc)
            case _ =>
              acc.reverse
          }

        import std.anyVal._
        loop(is.sorted, as.zipWithIndex, IList.empty[A])
      }
    }
  }

  def someOf[A](as: IList[A]): Gen[IList[A]] =
    choose(0, as.length).flatMap(i => pick(i, as))

  def promote[A, B](f: A => Gen[B]): Gen[A => B] =
    gen((i, r) => (a => f(a).f(i, r)._1, r))

  implicit val instance: Monad[Gen] =
    new Monad[Gen] {
      override def bind[A, B](fa: Gen[A])(f: A => Gen[B]) =
        fa flatMap f
      override def map[A, B](fa: Gen[A])(f: A => B) =
        fa map f
      override def point[A](a: => A) =
        Gen.value(a)
    }

  implicit val genBoolean: Gen[Boolean] =
    elements(true, false)

  val genIntSized: Gen[Int] =
    parameterised((i, r) => chooseR(-i, i, r))

  val genIntAll: Gen[Int] =
    gen((_, r) => r.nextInt)

  val genLongAll: Gen[Long] =
    gen((_, r) => r.nextLong)

  val genByteAll: Gen[Byte] =
    genIntAll.map(_.toByte)

  val genCharAll: Gen[Char] =
    genIntAll.map(_.toChar)

  val genByteSized: Gen[Byte] =
    genIntSized.map(_.toByte)

  val genCharSized: Gen[Char] =
    genIntSized.map(_.toChar)

  val genAsciiChar: Gen[Char] =
    choose('!', '~').map(_.toChar)

  val genAsciiString: Gen[String] =
    listOf(genAsciiChar).map(_.foldLeft("")(_ + _))

  implicit val genUnit: Gen[Unit] =
    value(())

  implicit def maybe[A](implicit A: Gen[A]): Gen[Maybe[A]] =
    Gen.frequency(
      1 -> Gen.value(Maybe.empty[A]),
      20 -> A.map(Maybe.just)
    )

  implicit def option[A](implicit A: Gen[A]): Gen[Option[A]] =
    maybe[A].map(_.toOption)

  implicit val genIntBoundaries: Gen[Int] =
    parameterised{ (_, r) =>
      frequency(
        1 -> value(0),
        1 -> value(1),
        1 -> value(-1),
        1 -> value(Int.MaxValue),
        1 -> value(Int.MaxValue - 1),
        1 -> value(Int.MinValue),
        1 -> value(Int.MinValue + 1),
        93 -> choose(Int.MinValue, Int.MaxValue)
      )
    }

  implicit val genLongBoundaries: Gen[Long] =
    parameterised{ (_, r) =>
      frequency(
        1 -> value(0L),
        1 -> value(1L),
        1 -> value(-1L),
        1 -> value(Long.MaxValue),
        1 -> value(Long.MaxValue - 1),
        1 -> value(Long.MinValue),
        1 -> value(Long.MinValue + 1),
        93 -> genLongAll
      )
    }

  implicit val genByteBoundaries: Gen[Byte] =
    parameterised{ (_, r) =>
      frequency(
        1 -> value(0: Byte),
        1 -> value(1: Byte),
        1 -> value(-1: Byte),
        1 -> value(Byte.MaxValue),
        1 -> value(126: Byte),
        1 -> value(Byte.MinValue),
        1 -> value(-127: Byte),
        93 -> genByteAll
      )
    }

  implicit def lazyTuple2[A1, A2](implicit A1: Gen[A1], A2: Gen[A2]): Gen[LazyTuple2[A1, A2]] =
    Apply[Gen].apply2(A1, A2)(LazyTuple2(_, _))

  implicit def lazyTuple3[A1, A2, A3](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3]): Gen[LazyTuple3[A1, A2, A3]] =
    Apply[Gen].apply3(A1, A2, A3)(LazyTuple3(_, _, _))

  implicit def lazyTuple4[A1, A2, A3, A4](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4]): Gen[LazyTuple4[A1, A2, A3, A4]] =
    Apply[Gen].apply4(A1, A2, A3, A4)(LazyTuple4(_, _, _, _))

  implicit def lazyEitherGen[A1, A2](implicit A1: Gen[A1], A2: Gen[A2]): Gen[LazyEither[A1, A2]] =
    oneOf(
      A1.map(LazyEither.lazyLeft[A2].apply(_)),
      A2.map(LazyEither.lazyRight[A1].apply(_))
    )

  implicit def tuple1[A1](implicit A1: Gen[A1]): Gen[Tuple1[A1]] =
    A1.map(Tuple1.apply)

  implicit def imapGen[A: Order: Gen, B: Gen]: Gen[A ==>> B] =
    Gen[List[(A, B)]].map(==>>.fromList(_))

  implicit def isetGen[A: Order: Gen]: Gen[ISet[A]] =
    Gen[List[A]].map(ISet.fromList(_))

  implicit def kleisli[F[_], A, B](implicit F: Gen[A => F[B]]): Gen[Kleisli[F, A, B]] =
    F.map(Kleisli(_))

  implicit def cokleisli[F[_], A, B](implicit F: Gen[F[A] => B]): Gen[Cokleisli[F, A, B]] =
    F.map(Cokleisli(_))

  implicit def nullResult[A, B](implicit F: Gen[A => Option[B]]): Gen[NullResult[A, B]] =
    F.map(NullResult(_))

  implicit def nullArgument[A, B](implicit F: Gen[Option[A] => B]): Gen[NullArgument[A, B]] =
    F.map(NullArgument(_))

  implicit def nonEmptyList[A](implicit A: Gen[A]): Gen[NonEmptyList[A]] =
    Apply[Gen].apply2(A, Gen[List[A]])(NonEmptyList.nel)

  implicit def oneAnd[F[_], A](implicit F: Gen[F[A]], A: Gen[A]): Gen[OneAnd[F, A]] =
    Apply[Gen].apply2(A, F)(OneAnd(_, _))

  implicit def oneOr[F[_], A](implicit F: Gen[F[A]], A: Gen[A]): Gen[OneOr[F, A]] =
    Gen[F[A] \/ A].map(OneOr.apply)

  implicit def dequeueGen[A](implicit A: Gen[A]): Gen[Dequeue[A]] =
    Gen.oneOf(
      Gen.value(Dequeue.empty[A]),
      A.map(Dequeue.apply(_)),
      Apply[Gen].apply2(Gen[NonEmptyIList[A]], Gen[NonEmptyIList[A]])(
        (x, y) => Dequeue.fromFoldable(x) ++ Dequeue.fromFoldable(y)
      )
    )

  implicit def disjunction[A, B](implicit A: Gen[A], B: Gen[B]): Gen[A \/ B] =
    oneOf(A.map(\/.left), B.map(\/.right))

  implicit def eitherTGen[F[_], A, B](implicit F: Gen[F[A \/ B]]): Gen[EitherT[F, A, B]] =
    F.map(EitherT(_))

  implicit def maybeTGen[F[_], A](implicit F: Gen[F[Maybe[A]]]): Gen[MaybeT[F, A]] =
    F.map(MaybeT.apply(_))

  implicit def optionTGen[F[_], A](implicit F: Gen[F[Option[A]]]): Gen[OptionT[F, A]] =
    F.map(OptionT.apply(_))

  implicit def coproductGen[F[_], G[_], A](implicit F: Gen[F[A] \/ G[A]]): Gen[Coproduct[F, G, A]] =
    F.map(Coproduct(_))

  implicit def constGen[A, B](implicit A: Gen[A]): Gen[Const[A, B]] =
    A.map(Const(_))

  implicit def writerTGen[F[_], A, B](implicit F: Gen[F[(A, B)]]): Gen[WriterT[F, A, B]] =
    F.map(WriterT(_))

  implicit def unwriterTGen[F[_], A, B](implicit F: Gen[F[(A, B)]]): Gen[UnwriterT[F, A, B]] =
    F.map(UnwriterT(_))

  implicit def indexedStateTGen[F[_], S1, S2, A](implicit F: Gen[S1 => F[(S2, A)]]): Gen[IndexedStateT[F, S1, S2, A]] =
    F.map(IndexedStateT(_))

  implicit def indexedContsTGen[W[_], M[_], R, O, A](implicit F: Gen[W[A => M[O]] => M[R]]): Gen[IndexedContsT[W, M, R, O, A]] =
    F.map(IndexedContsT(_))

  implicit def indexedReaderWriterStateTGen[F[_], R, W, S1, S2, A](implicit F: Gen[(R, S1) => F[(W, A, S2)]]): Gen[IndexedReaderWriterStateT[F, R, W, S1, S2, A]] =
    F.map(IndexedReaderWriterStateT.apply)

  implicit def streamTGen[F[_]: Applicative, A](implicit F: Gen[F[Stream[A]]]): Gen[StreamT[F, A]] =
    F.map(StreamT.fromStream(_))

  implicit def theseGen[A, B](implicit A: Gen[A], B: Gen[B]): Gen[A \&/ B] =
    Gen.oneOf(
      A.map(\&/.This.apply),
      B.map(\&/.That.apply),
      Apply[Gen].apply2(A, B)(\&/.Both.apply)
    )

  implicit def listTGen[F[_], A](implicit F: Gen[F[List[A]]]): Gen[ListT[F, A]] =
    F.map(ListT.apply(_))

  implicit def dlistGen[A: Gen]: Gen[DList[A]] =
    Gen[List[A]].map(DList.fromList(_))

  implicit def heapGen[A: Gen: Order]: Gen[Heap[A]] =
    Gen[IList[A]].map(Heap.fromCodata(_))

  implicit def monoidCoproduct[A: Gen, B: Gen]: Gen[A :+: B] =
    Gen[Vector[A \/ B]].map(new :+:(_))


  implicit def indexedStoreTGen[F[_], I: Gen, A, B](implicit F: Gen[F[A => B]]): Gen[IndexedStoreT[F, I, A, B]] =
    Gen[(F[A => B], I)].map(IndexedStoreT(_))

  implicit val orderingGen: Gen[Ordering] =
    elements(
      scalaz.Ordering.GT,
      scalaz.Ordering.EQ,
      scalaz.Ordering.LT
    )

  implicit def validationGen[A: Gen, B: Gen]: Gen[Validation[A, B]] =
    Gen[A \/ B].map(_.validation)

  implicit def zipperGen[A: Gen]: Gen[Zipper[A]] =
    Gen[(Stream[A], A, Stream[A])].map{case (a, b, c) => Zipper(a, b, c)}

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

  implicit def needGen[A](implicit A: Gen[A]): Gen[Need[A]] =
    A.map(Need.apply(_))

  implicit def futureGen[A](implicit A: Gen[A]): Gen[Future[A]] =
    A.map(Future.successful(_))

  implicit def endoGen[A: Gen: Cogen]: Gen[Endo[A]] =
    Gen[A => A].map(Endo(_))

  implicit def kleisliLikeEndoGen[G[_[_], _, _], F[_], A](implicit F: Gen[G[F, A, A]]): Gen[Endomorphic[({type l[a, b] = G[F, a, b]})#l, A]] =
    endomorphicGen[({type l[a, b] = G[F, a, b]})#l, A]

  implicit def contravariantCoyonedaGen[F[_], A](implicit F: Gen[F[A]]): Gen[ContravariantCoyoneda[F, A]] =
    F.map(ContravariantCoyoneda.lift)

  implicit def fingerGen[V, A](implicit A: Gen[A], R: Reducer[A, V]): Gen[Finger[V, A]] =
    Gen.oneOf(
      A.map(FingerTree.one[V, A]),
      Apply[Gen].apply2(A, A)(FingerTree.two[V, A]),
      Apply[Gen].apply3(A, A, A)(FingerTree.three[V, A]),
      Apply[Gen].apply4(A, A, A, A)(FingerTree.four[V, A])
    )
}
