package scalaprops

import scala.util.Random
import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._
import scalaz.std.tuple._

object GenTest extends Scalaprops {

  private implicit def genGen[A](implicit A: Gen[A]): Gen[Gen[A]] = {
    val values = Gen[List[A]].f(100, Rand.standard(Random.nextLong()))._1
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
            val (a, r0) = A.f(i, r)
            (a, r0.reseed(x))
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
          Equal[(A, Rand)].equal(x.f(size, r), y.f(size, r))
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

    Property.forAll[List[Rand], List[Int]]{ (rs, xs) =>
      val g = Gen.elements(xs.head, xs.tail: _*)
      val r = rs.map(r => g.f(Int.MaxValue, r)._1)
      (r.toSet == xs.toSet) && (xs.toSet.size == N)
    }(
      Gen.sequenceNList(1000, Gen[Rand]),
      Gen.sequenceNList(5, Gen[Int])
    ).toCheckWith(Param.rand(Rand.fromSeed())).toProperties("test Gen.element")
  }

  val `test Gen.sequenceNList` = {
    val min = 5
    val max = 30
    val a = - 500
    val b = 20000

    Property.forAll[(Int, List[Int])]{ case (size, values) =>
      (values.length == size) && (min <= size && size <= max) && values.forall{
        x => a <= x && x <= b
      }
    }(
      Gen.choose(min, max).flatMap{ size =>
        Gen.sequenceNList(size, Gen.choose(a, b)).map(size -> _)
      }
    )
  }

  val `test Gen.frequencey` =
    Property.forAll[List[Boolean]] { list =>
      val (t, f) = list.partition(identity)
      (t.size < f.size) && t.nonEmpty
    }(
      Gen.sequenceNList(100, Gen.frequency(
        1 -> Gen.value(true),
        5 -> Gen.value(false)
      ))
    )

  val maybeGen = Property.forAll[Int, Int]{ (size, seed) =>
    val values = Gen[Maybe[Int]].samples(size = size, seed = seed)
    val just = values.count(_.isJust)
    (values.size == size) && (just > (size / 2)) && (just < size)
  }(Gen.choose(100, 10000), Gen[Int])

}
