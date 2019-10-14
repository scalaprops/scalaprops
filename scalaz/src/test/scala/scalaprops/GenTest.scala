package scalaprops

import scala.util.Random
import scalaz._
import scalaz.std.anyVal._
import scalaz.std.tuple._
import ScalapropsScalaz._

object GenTest extends Scalaprops {

  implicit def genGen[A](implicit A: Gen[A]): Gen[Gen[A]] = {
    val values = Gen[List[A]].sample(size = 100, seed = Random.nextLong())
    Gen.oneOf(
      Gen.value(A),
      List(
        values.map(
          x => Gen.value(Gen.value(x))
        ),
        List.fill(100)(Random.nextInt(Int.MaxValue - 1) + 1).map(x => Gen.value(A.resize(x))),
        List
          .fill(100)(Random.nextInt(Int.MaxValue - 1) + 1)
          .map(
            x =>
              Gen.value(Gen.gen { (i, r) =>
                val (r0, a) = A.f(i, r)
                (r0.reseed(x), a)
              })
          )
      ).flatten: _*
    )
  }

  implicit def genEqual[A](implicit A: Equal[A]): Equal[Gen[A]] =
    Equal.equal { (x, y) =>
      Iterator.fill(100)((Random.nextInt(), Random.nextLong())).forall {
        case (size, seed) =>
          val r = Rand.standard(seed)
          Equal[(Rand, A)].equal(x.f(size, r), y.f(size, r))
      }
    }

  val testLaw =
    Properties.list(
      scalazlaws.monad.all[Gen],
      scalazlaws.bindRec.all[Gen],
      scalazlaws.equal.all[Gen[Int]]
    )

  val `test Gen.elements` = {
    val N = 5

    Property
      .forAllG(
        Gen.sequenceNList(10000, Gen[Rand]),
        Gen.sequenceNList(N, Gen[Int])
      ) { (rs, xs) =>
        val g = Gen.elements(xs.head, xs.tail: _*)
        val r = rs.map(r => g.f(Int.MaxValue, r)._2)
        (r.toSet == xs.toSet) && (xs.toSet.size == N)
      }
      .toProperties((), Param.rand(Rand.fromSeed()))
  }

  val `test Gen.sequenceNList` = {
    val min = 5
    val max = 30
    val a = -500
    val b = 20000

    Property.forAllG(
      Gen.choose(min, max).flatMap { size =>
        Gen.sequenceNList(size, Gen.choose(a, b)).map(size -> _)
      }
    ) {
      case (size, values) =>
        (values.length == size) && (min <= size && size <= max) && values.forall { x =>
          a <= x && x <= b
        }
    }
  }

  val `test Gen.frequencey` =
    Property.forAllG(
      Gen.sequenceNList(
        100,
        Gen.frequency(
          1 -> Gen.value(true),
          5 -> Gen.value(false)
        )
      )
    ) { list =>
      val (t, f) = list.partition(identity)
      (t.size < f.size) && t.nonEmpty
    }

  val testMaybeGen = Property.forAllG(
    Gen[Int],
    Gen.choose(500, 10000),
    Gen[Int]
  ) { (size, listSize, seed) =>
    val values = Gen[Maybe[Int]].samples(size = size, listSize = listSize, seed = seed)
    val just = values.count(_.isJust)
    (values.size == listSize) && (just > (listSize / 2)) && (just < listSize)
  }

  val choose = Property.forAll { (a: Int, b: Int, size: Int, seed: Long) =>
    val x = Gen.choose(a, b).sample(size = size, seed = seed)
    val max = math.max(a, b)
    val min = math.min(a, b)
    (min <= x) && (x <= max)
  }

  val listOfN_1 = Property.forAll { (size0: Byte, seed: Long) =>
    val size = math.abs(size0.toInt)
    val result = Gen.listOfN(size, Gen[Unit]).map(_.size).sample(seed = seed)
    result <= size
  }

  val listOfN_2 = Property.forAll { seed: Long =>
    val size = 3
    Gen.listOfN(size, Gen[Unit]).map(_.size).samples(seed = seed, listSize = 100).distinct.size == (size + 1)
  }

  val arrayOfN = Property.forAll { (size0: Byte, seed: Long) =>
    val size = math.abs(size0.toInt)
    val result = Gen.arrayOfN(size, Gen[Unit]).map(_.size).sample(seed = seed)
    result <= size
  }

  val posLong = Property.forAllG(Gen.positiveLong) { _ > 0 }
  val posInt = Property.forAllG(Gen.positiveInt) { _ > 0 }
  val posShort = Property.forAllG(Gen.positiveShort) { _ > 0 }
  val posByte = Property.forAllG(Gen.positiveByte) { _ > 0 }
  val posFloat = Property.forAllG(Gen.positiveFloat) { _ > 0.0f }
  val posDouble = Property.forAllG(Gen.positiveDouble) { _ > 0.0d }
  val posFiniteFloat = Property.forAllG(Gen.positiveFiniteFloat) { f =>
    f > 0.0f && !f.isInfinity
  }
  val posFiniteDouble = Property.forAllG(Gen.positiveFiniteDouble) { d =>
    d > 0.0d && !d.isInfinity
  }

  val negLong = Property.forAllG(Gen.negativeLong) { _ < 0 }
  val negInt = Property.forAllG(Gen.negativeInt) { _ < 0 }
  val negShort = Property.forAllG(Gen.negativeShort) { _ < 0 }
  val negByte = Property.forAllG(Gen.negativeByte) { _ < 0 }
  val negFloat = Property.forAllG(Gen.negativeFloat) { _.compare(0.0f) < 0 }
  val negDouble = Property.forAllG(Gen.negativeDouble) { _.compare(0.0d) < 0 }
  val negFiniteFloat = Property.forAllG(Gen.negativeFiniteFloat) { f =>
    f.compare(0.0f) < 0 && !f.isInfinity
  }
  val negFiniteDouble = Property.forAllG(Gen.negativeFiniteDouble) { d =>
    d.compare(0.0d) < 0 && !d.isInfinity
  }

  val nonNegLong = Property.forAllG(Gen.nonNegativeLong) { 0 <= _ }
  val nonNegInt = Property.forAllG(Gen.nonNegativeInt) { 0 <= _ }
  val nonNegShort = Property.forAllG(Gen.nonNegativeShort) { 0 <= _ }
  val nonNegByte = Property.forAllG(Gen.nonNegativeByte) { 0 <= _ }
  val nonNegFloat = Property.forAllG(Gen.nonNegativeFloat) { f =>
    f.compare(0.0f) >= 0 && !f.isNaN
  }
  val nonNegDouble = Property.forAllG(Gen.nonNegativeDouble) { f =>
    f.compare(0.0d) >= 0 && !f.isNaN
  }
  val nonNegFiniteFloat = Property.forAllG(Gen.nonNegativeFiniteFloat) { f =>
    f.compare(0.0f) >= 0 && !f.isInfinity && !f.isNaN
  }
  val nonNegFiniteDouble = Property.forAllG(Gen.nonNegativeFiniteDouble) { d =>
    d.compare(0.0d) >= 0 && !d.isInfinity && !d.isNaN
  }

  val finiteFloat = Property.forAllG(Gen.genFiniteFloat) { f =>
    !f.isInfinity && !f.isNaN
  }
  val finiteDouble = Property.forAllG(Gen.genFiniteDouble) { d =>
    !d.isInfinity && !d.isNaN
  }

  val genFunction = {
    def test1[A: Gen: Cogen](name: String) =
      test[A, A](s"$name => $name")

    def test[A: Gen: Cogen, B: Gen](name: String) =
      Property.forAll { seed: Int =>
        val size = 10
        val as = Gen[A].infiniteStream(seed = seed).distinct.take(5).toList
        val values = Gen[A => B].samples(listSize = size, seed = seed).map(as.map(_))
        val set = values.toSet
        val result = set.size == size
        assert(result, s"$name ${set.size} $as $set $values")
        result
      }.toProperties(name)

    Properties
      .list(
        test1[Long]("Long"),
        test1[Int]("Int"),
        test1[Byte]("Byte"),
        test1[Short]("Short"),
        test1[Either[Byte, Short]]("Either[Byte, Short]"),
        test1[Long \/ Int]("""(Long \/ Int)"""),
        test1[(Int, Byte)]("Tuple2[Int, Byte]"),
        test1[Option[Int]]("Option[Int]"),
        test1[Map[Int, Int]]("Map[Int, Int]").andThenParam(Param.maxSize(10) andThen Param.minSuccessful(5)),
        test1[Int ==>> Int]("(Int ==>> Int)").andThenParam(Param.maxSize(10) andThen Param.minSuccessful(5)),
        test1[IList[Int]]("IList[Int]").andThenParam(Param.maxSize(10)),
        test1[Byte \&/ Byte]("""Byte \&/ Byte)"""),
        test[Int, List[Int]]("Int => List[Int]"),
        test[List[Int], Int]("List[Int] => Int").andThenParam(Param.maxSize(20)),
        test[Option[Byte], Byte]("Option[Byte] => Byte"),
        test[Byte, Option[Byte]]("Byte => Option[Byte]"),
        test[Either[Byte, Boolean], Int]("Either[Byte, Boolean] => Int"),
        test[Int, Map[Int, Boolean]]("Int => Map[Int, Boolean]").andThenParam(Param.minSuccessful(5))
      )
      .composeParam(Param.minSuccessful(20))
  }

  val functionGenTest = {

    def permutations[A](xs: IList[A], n: Int): IList[IList[A]] =
      if (xs.isEmpty) {
        IList.empty
      } else {
        def f(ls: IList[A], rest: IList[A], n: Int): IList[IList[A]] =
          if (n == 0) IList(ls) else rest.flatMap(v => f(v :: ls, rest, n - 1))
        f(IList.empty, xs, n)
      }

    def combinations[A: Order, B](as: IList[A], bs: IList[B]): IList[A ==>> B] = {
      val xs = permutations(bs, as.length)
      IList.fill(xs.length)(as).zip(xs).map {
        case (keys, values) =>
          keys.zip(values).toMap
      }
    }

    val defaultSize = 10000

    // https://github.com/scala/bug/issues/10883
    implicit class Scala_2_13_Stream_Bug[A](self: Stream[A]) {
      def take_scala_10883_workaround(n: Int): Stream[A] = {
        if (n <= 0 || self.isEmpty) Stream.empty
        else if (n == 1) Stream.cons(self.head, Stream.empty)
        else Stream.cons(self.head, self.tail take_scala_10883_workaround n - 1)
      }
    }

    def test[A: Cogen: Order, B: Gen: Order](
      domain: IList[A],
      codomain: IList[B],
      name: String,
      streamSize: Int = defaultSize
    ) = {
      import scalaz.std.stream._
      val size = List.fill(domain.length)(codomain.length).product

      Property.forAll { seed: Long =>
        val x = IList
          .fromFoldable(
            Gen[A => B]
              .infiniteStream(seed = seed)
              .map { f =>
                IMap.fromFoldable(domain.map(a => a -> f(a)))
              }
              .take_scala_10883_workaround(streamSize)
              .distinct
              .take_scala_10883_workaround(size)
          )
          .sorted

        assert(x.length == size, s"${x.length} != $size")
        Equal[IList[A ==>> B]].equal(x, combinations(domain, codomain).sorted)
      }.toProperties(name)
    }

    def test1[A: Cogen: Order: Gen](values: IList[A], name: String, streamSize: Int = defaultSize) =
      test[A, A](values, values, s"($name) => ($name)", streamSize)

    val orderingValues = IList[Ordering](Ordering.EQ, Ordering.GT, Ordering.LT)

    Properties.list(
      test1(IList(true, false), "Boolean"),
      test1(orderingValues, "Ordering"),
      test(
        IList(true, false),
        IList(Maybe.just(true), Maybe.just(false), Maybe.empty[Boolean]),
        "Boolean => Maybe[Boolean]"
      ),
      test(IList(true, false), orderingValues, "Boolean => Ordering"),
      test(orderingValues, IList(true, false), "Ordering => Boolean"),
      test(
        IList(Maybe.just(true), Maybe.just(false), Maybe.empty[Boolean]),
        IList(true, false),
        "Maybe[Boolean] => Boolean"
      ),
      test1(IList(Maybe.just(true), Maybe.just(false), Maybe.empty[Boolean]), "Maybe[Boolean]", 100000)
        .andThenParam(Param.minSuccessful(3)),
      test1(IList(true, false).flatMap(a => IList(\/.right(a), \/.left(a))), """Boolean \/ Boolean""", 50000)
        .andThenParam(Param.minSuccessful(3))
    )
  }

}
