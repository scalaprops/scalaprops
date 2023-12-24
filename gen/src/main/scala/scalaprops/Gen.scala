package scalaprops

import Gen.gen
import scala.concurrent.Future

final case class Gen[A] private (f: (Int, Rand) => (Rand, A)) {
  def map[B](g: A => B): Gen[B] =
    gen { (i, r) =>
      val (r0, a) = f(i, r)
      (r0, g(a))
    }

  def mapOrId(g: PartialFunction[A, A]): Gen[A] =
    map(a => g.applyOrElse(a, (_: A) => a))

  def flatMap[B](g: A => Gen[B]): Gen[B] =
    gen { (i, r) =>
      val (r0, a) = f(i, r)
      g(a).f(i, r0)
    }

  def resize(s: Int): Gen[A] =
    mapSize(Function.const(s))

  def mapSize(g: Int => Int): Gen[A] =
    gen((s, r) => f(g(s), r))

  /** convenience method for get sample values */
  def samples(size: Int = Gen.defaultSize, listSize: Int = 100, seed: Long = Rand.defaultSeed): List[A] =
    Gen.sequenceNList(listSize, this).f(size, Rand.standard(seed))._2

  def sample(size: Int = Gen.defaultSize, seed: Long = Rand.defaultSeed): A =
    f(size, Rand.standard(seed))._2

  def infiniteIterator(size: Int = Gen.defaultSize, seed: Long = Rand.defaultSeed): Iterator[A] =
    Gen.infinite(size, Rand.standard(seed), this)

  def infiniteStream(size: Int = Gen.defaultSize, seed: Long = Rand.defaultSeed): Stream[A] =
    Gen.infinite(size, Rand.standard(seed), this).toStream
}

sealed abstract class GenInstances0 extends GenInstances1 {}

object Gen extends GenInstances0 {
  private[scalaprops] val defaultSize = Platform.genSize

  private[scalaprops] val Int2Byte = (_: Int).toByte
  private[scalaprops] val Int2Short = (_: Int).toShort
  private[this] val Int2Char = (_: Int).toChar

  def gen[A](f: (Int, Rand) => (Rand, A)): Gen[A] =
    new Gen(f)

  def apply[A](implicit A: Gen[A]): Gen[A] = A

  def delay[A](g: => Gen[A]): Gen[A] = {
    lazy val g0 = g
    gen((size, r) => g0.f(size, r)) // should not change to `gen(g0.f)` !
  }

  def from[A1, Z](f: A1 => Z)(implicit A1: Gen[A1]): Gen[Z] =
    from1(f)(A1)

  def from1[A1, Z](f: A1 => Z)(implicit A1: Gen[A1]): Gen[Z] =
    A1.map(f)

  def value[A](a: A): Gen[A] =
    gen((_, r) => (r, a))

  implicit def f0[Z](implicit Z: Gen[Z]): Gen[Function0[Z]] =
    Z.map(z => () => z)

  implicit def f1[A1, Z](implicit A1: Cogen[A1], Z: Gen[Z]): Gen[A1 => Z] =
    Gen.gen { (i, r) => (r.next, a => A1.cogen(a, CogenState(r, Z)).gen.f(i, r)._2) }

  def oneOf[A](x: Gen[A], xs: Gen[A]*): Gen[A] = {
    val array = (x +: xs).toArray[Any]
    choose(0, xs.length).flatMap(array(_).asInstanceOf[Gen[A]])
  }

  def oneOfLazy[A](x: Lazy[Gen[A]], xs: Lazy[Gen[A]]*): Gen[A] = {
    val array = (x +: xs).toArray
    choose(0, xs.length).flatMap(array(_).value)
  }

  private[this] def sequenceN[F[_], A](n: Int, g: Gen[A], f: List[A] => F[A]): Gen[F[A]] =
    sequenceNList(n, g).map(f)

  def sequenceNArray[A: reflect.ClassTag](n: Int, g: Gen[A]): Gen[Array[A]] = {
    val array = new Array[A](n)
    @annotation.tailrec
    def loop(size: Int, i: Int, next: Rand): (Rand, Array[A]) = {
      if (i < n) {
        val r = g.f(size, next)
        array(i) = r._2
        loop(size, i + 1, r._1)
      } else {
        (next, array)
      }
    }
    gen { (size, r) => loop(size, 0, r) }
  }

  def sequenceNList[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    @annotation.tailrec
    def loop(size: Int, i: Int, next: Rand, acc: List[A]): (Rand, List[A]) = {
      if (i < n) {
        val r = g.f(size, next)
        loop(size, i + 1, r._1, r._2 :: acc)
      } else {
        (next, acc.reverse)
      }
    }
    gen { (size, r) => loop(size, 0, r, List.empty[A]) }
  }

  def infinite[A](genSize: Int, r: Rand, g: Gen[A]): Iterator[A] =
    new Iterator[A] {
      private[this] var rand = r
      override def next(): A = {
        val x = g.f(genSize, rand)
        rand = x._1
        x._2
      }
      override def hasNext = true
    }

  def parameterised[A](f: (Int, Rand) => Gen[A]): Gen[A] =
    gen((i, r) => f(i, r).f(i, r))

  def sized[A](f: Int => Gen[A]): Gen[A] =
    parameterised((i, _) => f(i))

  def chooseLong(from: Long, to: Long): Gen[Long] =
    gen { (_, r) => r.chooseLong(from, to) }

  def choose(from: Int, to: Int): Gen[Int] =
    gen { (_, r) => r.choose(from, to) }

  def chooseR(from: Int, to: Int, r: Rand): Gen[Int] =
    gen { (_, _) => r.choose(from, to) }

  @annotation.tailrec
  private[this] def pick0[B](n: Int, gs: List[(Int, B)]): B =
    gs match {
      case h :: t =>
        val k = h._1
        if (n <= k) h._2
        else pick0(n - k, t)
      case _ =>
        sys.error(s"bug? $n $gs")
    }

  def lazyFrequency[A](g: (Int, Lazy[Gen[A]]), gs: List[(Int, Lazy[Gen[A]])]): Gen[A] = {
    val x = g :: gs
    choose(1, x.iterator.map(_._1).sum).flatMap { i => pick0(i, x).value }
  }

  def frequency[A](g: (Int, Gen[A]), gs: (Int, Gen[A])*): Gen[A] = {
    val x = g :: gs.toList
    choose(1, x.iterator.map(_._1).sum).flatMap { i => pick0(i, x) }
  }

  def lazyFrequency[A](g: (Int, Lazy[Gen[A]]), gs: (Int, Lazy[Gen[A]])*): Gen[A] =
    lazyFrequency(g, gs.toList)

  def elemFrequency[A](a: (Int, A), as: List[(Int, A)]): Gen[A] =
    frequency((a._1, Gen.value(a._2)), as.map { case (i, a) => i -> Gen.value(a) }*)

  def elements[A](a: A, as: A*): Gen[A] = {
    val xs = (a +: as).toArray[Any]
    choose(0, as.length).map { xs(_).asInstanceOf[A] }
  }

  private[scalaprops] def listOf_[F[_], A](g: Gen[A], min: Int, f: List[A] => F[A]): Gen[F[A]] =
    parameterised { (size, r) => chooseR(min, size.max(min), r).flatMap { n => sequenceN(n, g, f) } }

  private[this] def arrayOf[A: reflect.ClassTag](g: Gen[A], min: Int): Gen[Array[A]] =
    parameterised { (size, r) => chooseR(min, size.max(min), r).flatMap { n => sequenceNArray(n, g) } }

  def listOf[A](g: Gen[A], min: Int = 0): Gen[List[A]] =
    listOf_[List, A](g, min, x => x)

  def listOfN[A](maxSize: Int, g: Gen[A]): Gen[List[A]] =
    choose(0, maxSize).flatMap { n => sequenceNList(n, g) }

  def arrayOfN[A: reflect.ClassTag](maxSize: Int, g: Gen[A]): Gen[Array[A]] =
    choose(0, maxSize).flatMap { n => sequenceNArray(n, g) }

  implicit def vector[A](implicit A: Gen[A]): Gen[Vector[A]] =
    listOf_[Vector, A](A, 0, _.toVector)

  implicit def mapGen[A: Gen, B: Gen]: Gen[Map[A, B]] =
    list[(A, B)].map(_.toMap)

  implicit def setGen[A: Gen]: Gen[Set[A]] =
    list[A].map(_.toSet)

  implicit def streamGen[A](implicit A: Gen[A]): Gen[Stream[A]] =
    listOf_[Stream, A](A, 0, _.toStream)

  implicit def list[A](implicit A: Gen[A]): Gen[List[A]] =
    listOf_[List, A](A, 0, _.toList)

  implicit def arrayGen[A: reflect.ClassTag: Gen]: Gen[Array[A]] =
    arrayOf(Gen[A], 0)

  def listOf1[A](g: Gen[A]): Gen[List[A]] =
    listOf(g, 1)

  private[this] def pick[A](n: Int, as: List[A]): Gen[List[A]] = {
    val len = as.length
    if (n < 0 || n > len) {
      sys.error(s"bug $n $as")
    } else {
      sequenceNList(n, choose(0, len - 1)).map { is =>
        @annotation.tailrec
        def loop(iis: List[Int], aas: List[(A, Int)], acc: List[A]): List[A] =
          (iis, aas) match {
            case (h1 :: t1, h2 :: t2) =>
              if (h1 == h2._2) loop(t1, t2, acc)
              else loop(iis, t2, h2._1 :: acc)
            case _ =>
              acc.reverse
          }

        loop(is.sorted, as.zipWithIndex, List.empty[A])
      }
    }
  }

  def someOf[A](as: List[A]): Gen[List[A]] =
    choose(0, as.length).flatMap(i => pick(i, as))

  implicit val genBoolean: Gen[Boolean] =
    elements(true, false)

  val genIntSized: Gen[Int] =
    parameterised((i, r) => chooseR(-i, i, r))

  val genIntAll: Gen[Int] =
    gen((_, r) => r.nextInt)

  val genLongAll: Gen[Long] =
    gen((_, r) => r.nextLong)

  def chooseIntBitsToFloat(from: Int, to: Int): Gen[Float] =
    Choose[Int].withBoundaries(from, to).map(java.lang.Float.intBitsToFloat)

  val negativeFloat: Gen[Float] =
    chooseIntBitsToFloat(0x80000000, 0xff800000)

  val positiveFloat: Gen[Float] =
    chooseIntBitsToFloat(1, 0x7f800000)

  val nonNegativeFloat: Gen[Float] =
    chooseIntBitsToFloat(0, 0x7f800000)

  val negativeFiniteFloat: Gen[Float] =
    chooseIntBitsToFloat(0x80000000, 0xff7fffff)

  val positiveFiniteFloat: Gen[Float] =
    chooseIntBitsToFloat(1, 0x7f7fffff)

  val nonNegativeFiniteFloat: Gen[Float] =
    chooseIntBitsToFloat(0, 0x7f7fffff)

  val genFiniteFloat: Gen[Float] =
    Gen.oneOf(negativeFiniteFloat, nonNegativeFiniteFloat)

  def chooseLongBitsToDouble(from: Long, to: Long): Gen[Double] =
    Choose[Long].withBoundaries(from, to).map(java.lang.Double.longBitsToDouble)

  val negativeDouble: Gen[Double] =
    chooseLongBitsToDouble(0x8000000000000000L, 0xfff0000000000000L)

  val positiveDouble: Gen[Double] =
    chooseLongBitsToDouble(1L, 0x7ff0000000000000L)

  val nonNegativeDouble: Gen[Double] =
    chooseLongBitsToDouble(0L, 0x7ff0000000000000L)

  val negativeFiniteDouble: Gen[Double] =
    chooseLongBitsToDouble(0x8000000000000000L, 0xffefffffffffffffL)

  val positiveFiniteDouble: Gen[Double] =
    chooseLongBitsToDouble(1L, 0x7fefffffffffffffL)

  val nonNegativeFiniteDouble: Gen[Double] =
    chooseLongBitsToDouble(0L, 0x7fefffffffffffffL)

  val genFiniteDouble: Gen[Double] =
    Gen.oneOf(negativeFiniteDouble, nonNegativeFiniteDouble)

  val genSmallBigInt: Gen[BigInt] =
    genLongAll.map(BigInt(_))

  val genLargeBigInt: Gen[BigInt] =
    for {
      n <- genIntAll
      x <- genSmallBigInt
    } yield x << (n & 0x7f)

  val genSmallBigDecimal: Gen[BigDecimal] =
    for {
      n <- genLongAll
      d <- genLongAll
    } yield BigDecimal(n) / (if (d == 0L) 1 else d)

  val genLargeBigDecimal: Gen[BigDecimal] =
    for {
      n <- genIntAll
      x <- genLargeBigInt
    } yield BigDecimal(x, ~(n & 0x7f))

  val genByteAll: Gen[Byte] =
    genIntAll.map(Int2Byte)

  val genCharAll: Gen[Char] =
    genIntAll.map(Int2Char)

  val genShortAll: Gen[Short] =
    genIntAll.map(Int2Short)

  val genByteSized: Gen[Byte] =
    genIntSized.map(Int2Byte)

  val genCharSized: Gen[Char] =
    genIntSized.map(Int2Char)

  val genShortSized: Gen[Short] =
    genIntSized.map(Int2Short)

  val positiveByte: Gen[Byte] =
    Choose[Int].withBoundaries(1, Byte.MaxValue).map(Int2Byte)

  val positiveShort: Gen[Short] =
    Choose[Int].withBoundaries(1, Short.MaxValue).map(Int2Short)

  val positiveInt: Gen[Int] =
    Choose[Int].withBoundaries(1, Int.MaxValue)

  val positiveLong: Gen[Long] =
    Choose[Long].withBoundaries(1, Long.MaxValue)

  val negativeByte: Gen[Byte] =
    Choose[Byte].withBoundaries(Byte.MinValue, -1)

  val negativeShort: Gen[Short] =
    Choose[Short].withBoundaries(Short.MinValue, -1)

  val negativeInt: Gen[Int] =
    Choose[Int].withBoundaries(Int.MinValue, -1)

  val negativeLong: Gen[Long] =
    Choose[Long].withBoundaries(Long.MinValue, -1)

  val nonNegativeByte: Gen[Byte] =
    Choose[Byte].withBoundaries(0, Byte.MaxValue)

  val nonNegativeShort: Gen[Short] =
    Choose[Short].withBoundaries(0, Short.MaxValue)

  val nonNegativeInt: Gen[Int] =
    Choose[Int].withBoundaries(0, Int.MaxValue)

  val nonNegativeLong: Gen[Long] =
    Choose[Long].withBoundaries(0, Long.MaxValue)

  val asciiChar: Gen[Char] =
    choose('!', '~').map(Int2Char)

  val numChar: Gen[Char] =
    choose('0', '9').map(Int2Char)

  val alphaUpperChar: Gen[Char] =
    choose('A', 'Z').map(Int2Char)

  val alphaLowerChar: Gen[Char] =
    choose('a', 'z').map(Int2Char)

  val alphaChar: Gen[Char] =
    Gen.oneOf(alphaLowerChar, alphaUpperChar)

  val alphaNumChar: Gen[Char] =
    Gen.oneOf(alphaLowerChar, alphaUpperChar, numChar)

  def genString(g: Gen[Char], min: Int = 0): Gen[String] =
    arrayOf(g, min.max(0)).map(String.valueOf)

  /** alias for `genString(g, min = 1)` */
  def nonEmptyString(g: Gen[Char]): Gen[String] =
    genString(g, 1)

  val numString: Gen[String] =
    genString(numChar)

  val alphaUpperString: Gen[String] =
    genString(alphaUpperChar)

  val alphaLowerString: Gen[String] =
    genString(alphaLowerChar)

  val alphaString: Gen[String] =
    genString(alphaChar)

  val alphaNumString: Gen[String] =
    genString(alphaNumChar)

  val asciiString: Gen[String] =
    genString(asciiChar)

  implicit val genUnit: Gen[Unit] =
    value(())

  implicit def option[A](implicit A: Gen[A]): Gen[Option[A]] =
    Gen.frequency(
      1 -> Gen.value(Option.empty[A]),
      20 -> A.map(Some(_): Option[A])
    )

  implicit val genIntBoundaries: Gen[Int] =
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

  implicit val genLongBoundaries: Gen[Long] =
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

  implicit val genFloatBoundaries: Gen[Float] =
    frequency(
      1 -> value(Float.NaN),
      1 -> value(Float.PositiveInfinity),
      1 -> value(Float.NegativeInfinity),
      1 -> value(0f),
      1 -> value(-0f),
      1 -> value(1f),
      1 -> value(-1f),
      1 -> value(Float.MinPositiveValue),
      1 -> value(-Float.MinPositiveValue),
      50 -> genIntAll.map(_.toFloat),
      41 -> genFiniteFloat
    )

  implicit val genDoubleBoundaries: Gen[Double] =
    frequency(
      1 -> value(Double.NaN),
      1 -> value(Double.PositiveInfinity),
      1 -> value(Double.NegativeInfinity),
      1 -> value(0f),
      1 -> value(-0f),
      1 -> value(1f),
      1 -> value(-1f),
      1 -> value(Double.MinPositiveValue),
      1 -> value(-Double.MinPositiveValue),
      50 -> genLongAll.map(_.toDouble),
      41 -> genFiniteDouble
    )

  implicit val genBigIntBoundaries: Gen[BigInt] =
    frequency(
      1 -> value(BigInt(0)),
      1 -> value(BigInt(1)),
      1 -> value(BigInt(-1)),
      1 -> value(BigInt(Int.MaxValue) + 1),
      1 -> value(BigInt(Int.MinValue) - 1),
      1 -> value(BigInt(Long.MaxValue) - 1),
      1 -> value(BigInt(Long.MinValue) + 1),
      1 -> value(BigInt(Long.MaxValue)),
      1 -> value(BigInt(Long.MinValue)),
      1 -> value(BigInt(Long.MaxValue) + 1),
      1 -> value(BigInt(Long.MinValue) - 1),
      70 -> genSmallBigInt,
      19 -> genLargeBigInt
    )

  implicit val genBigInteger: Gen[java.math.BigInteger] =
    genBigIntBoundaries.map(_.bigInteger)

  implicit val genBigDecimalBoundaries: Gen[BigDecimal] =
    frequency(
      1 -> value(BigDecimal(0)),
      1 -> value(BigDecimal("1e-600")),
      1 -> value(BigDecimal("-1e-600")),
      1 -> value(BigDecimal(1)),
      1 -> value(BigDecimal(-1)),
      1 -> value(BigDecimal(Double.MaxValue) * 10),
      1 -> value(BigDecimal(Double.MinValue) * 10),
      70 -> genSmallBigDecimal,
      19 -> genLargeBigDecimal
    )

  implicit val genJavaBigDecimal: Gen[java.math.BigDecimal] =
    genBigDecimalBoundaries.map(_.bigDecimal)

  implicit val genByteBoundaries: Gen[Byte] =
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

  implicit val genShortBoundaries: Gen[Short] =
    frequency(
      1 -> value(0: Short),
      1 -> value(1: Short),
      1 -> value(-1: Short),
      1 -> value(Short.MaxValue),
      1 -> value((Short.MaxValue - 1).asInstanceOf[Short]),
      1 -> value(Short.MinValue),
      1 -> value((Short.MinValue + 1).asInstanceOf[Short]),
      93 -> genShortAll
    )

  implicit val javaIntegerGen: Gen[java.lang.Integer] =
    Gen[Int].map(Integer.valueOf)

  implicit val javaLongGen: Gen[java.lang.Long] =
    Gen[Long].map(java.lang.Long.valueOf)

  implicit val javaByteGen: Gen[java.lang.Byte] =
    Gen[Byte].map(java.lang.Byte.valueOf)

  implicit val javaShortGen: Gen[java.lang.Short] =
    Gen[Short].map(java.lang.Short.valueOf)

  implicit val javaDoubleGen: Gen[java.lang.Double] =
    Gen[Double].map(java.lang.Double.valueOf)

  implicit val javaFloatGen: Gen[java.lang.Float] =
    Gen[Float].map(java.lang.Float.valueOf)

  implicit val javaBooleanGen: Gen[java.lang.Boolean] =
    Gen.elements(java.lang.Boolean.TRUE, java.lang.Boolean.FALSE)

  implicit def tuple1[A1](implicit A1: Gen[A1]): Gen[Tuple1[A1]] =
    A1.map(Tuple1.apply)

  implicit def eitherGen[A, B](implicit A: Gen[A], B: Gen[B]): Gen[A Either B] =
    oneOf(B.map(Right(_)), A.map(Left(_)))

  implicit def futureGen[A](implicit A: Gen[A]): Gen[Future[A]] =
    A.map(Future.successful(_))

  implicit def partialFunctionGen[A: Cogen, B: Gen]: Gen[PartialFunction[A, B]] =
    Gen[A => Option[B]].map(Function.unlift)

  def apply2[A1, A2, B](g1: Gen[A1], g2: Gen[A2])(f: (A1, A2) => B): Gen[B] =
    for {
      a1 <- g1
      a2 <- g2
    } yield f(a1, a2)
}
