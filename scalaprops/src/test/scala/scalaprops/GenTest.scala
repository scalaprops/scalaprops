package scalaprops

import scala.util.Random
import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._
import scalaz.std.tuple._

object GenTest extends Scalaprops {

  private implicit def genGen[A](implicit A: Gen[A]): Gen[Gen[A]] = {
    val values = Gen[List[A]].f(100, Rand.standard(Random.nextLong()))._2
    Gen.oneOf(
      Gen.value(A),
      List(
        values.map(
          x => Gen.value(Gen.value(x))
        ),
        List.fill(100)(Random.nextInt(Int.MaxValue - 1) + 1).map(x =>
          Gen.value(A.resize(x))
        ),
        List.fill(100)(Random.nextInt(Int.MaxValue - 1) + 1).map(x =>
          Gen.value(Gen.gen{ (i, r) =>
            val (r0, a) = A.f(i, r)
            (r0.reseed(x), a)
          })
        )
      ).flatten : _*
    )
  }

  implicit def genEqual[A](implicit A: Equal[A]): Equal[Gen[A]] =
    Equal.equal{ (x, y) =>
      Iterator.fill(100)((Random.nextInt(), Random.nextLong())).forall{
        case (size, seed) =>
          val r = Rand.standard(seed)
          Equal[(Rand, A)].equal(x.f(size, r), y.f(size, r))
      }
    }

  val testLaw =
    Properties.either(
      "Gen",
      scalazlaws.monad.all[Gen],
      scalazlaws.equal.all[Gen[Int]]
    )

  val `test Gen.elements` = {
    val N = 5

    Property.forAllG(
      Gen.sequenceNList(1000, Gen[Rand]),
      Gen.sequenceNList(5, Gen[Int])
    ){ (rs, xs) =>
      val g = Gen.elements(xs.head, xs.tail: _*)
      val r = rs.map(r => g.f(Int.MaxValue, r)._2)
      (r.toSet == xs.toSet) && (xs.toSet.size == N)
    }.toCheckWith(Param.rand(Rand.fromSeed())).toProperties("test Gen.element")
  }

  val `test Gen.sequenceNList` = {
    val min = 5
    val max = 30
    val a = - 500
    val b = 20000

    Property.forAllG(
      Gen.choose(min, max).flatMap{ size =>
        Gen.sequenceNList(size, Gen.choose(a, b)).map(size -> _)
      }
    ){ case (size, values) =>
      (values.length == size) && (min <= size && size <= max) && values.forall{
        x => a <= x && x <= b
      }
    }
  }

  val `test Gen.frequencey` =
    Property.forAllG(
      Gen.sequenceNList(100, Gen.frequency(
        1 -> Gen.value(true),
        5 -> Gen.value(false)
      ))
    ){ list =>
      val (t, f) = list.partition(identity)
      (t.size < f.size) && t.nonEmpty
    }

  val maybeGen = Property.forAllG(
    Gen[Int], Gen.choose(100, 10000), Gen[Int]
  ){ (size, listSize, seed) =>
    val values = Gen[Maybe[Int]].samples(size = size, listSize = listSize, seed = seed)
    val just = values.count(_.isJust)
    (values.size == listSize) && (just > (listSize / 2)) && (just < listSize)
  }

  val choose = Property.forAll{ (a: Int, b: Int, size: Int, seed: Int) =>
    val x = Gen.choose(a, b).f(size, Rand.fromSeed(seed))._2
    val max = math.max(a, b)
    val min = math.min(a, b)
    (min <= x) && (x <= max)
  }

  val listOfN_1 = Property.forAll{ (size0: Byte, seed: Long) =>
    val size = math.abs(size0.toInt)
    val result = Gen.listOfN(size, Gen[Unit]).map(_.size).sample(seed = seed)
    result <= size
  }

  val listOfN_2 = Property.forAll{ seed: Long =>
    val size = 3
    Gen.listOfN(size, Gen[Unit]).map(_.size).samples(seed = seed, listSize = 100).distinct.size == (size + 1)
  }

  val arrayOfN = Property.forAll{ (size0: Byte, seed: Long) =>
    val size = math.abs(size0.toInt)
    val result = Gen.arrayOfN(size, Gen[Unit]).map(_.size).sample(seed = seed)
    result <= size
  }

}
