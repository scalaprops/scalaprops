package scalaprops

import scalaz._
import scalaz.std.AllInstances._
import LazyEitherTest.lazyEitherEqual
import GenTags._

object GenCogenTest extends Scalaprops {

  def test[A: Cogen](implicit G: Gen[A], E: Equal[A]) =
    Property.forAll {
      val n = 100
      val seed = Gen[Int].sample(seed = System.currentTimeMillis())
      val values = G.samples(listSize = n, seed = seed).distinct
      val function = Gen[A => A].sample(seed = System.currentTimeMillis())
      val results = values.map(function).distinct
      print(s": ${values.size} ${results.size} ")
      if(values.size < (n / 3)){
        print(Console.RED + s"values ${values.size} " + Console.RESET)
      }
      if(results.size < (values.size / 3)){
        print(Console.RED + s"results ${results.size} " + Console.RESET)
      }
      println
      true
    }

  def test1[F[_]](implicit G: Gen[F[Int]], C: Cogen[F[Int]], E: Equal[F[Int]]) =
    test[F[Int]]

  def test2[F[_, _]](implicit G: Gen[F[Int, Int]], C: Cogen[F[Int, Int]], E: Equal[F[Int, Int]]) =
    test[F[Int, Int]]

  val byte = test[Byte]
  val short = test[Short]
  val int = test[Int]
  val long = test[Long]
  val const = test2[Const]
  val coproduct = test[Coproduct[Maybe, IList, Int]]
  val coyoneda = test[Coyoneda[Maybe, Int]]
  val dequeue = test1[Dequeue]
  val diev = test1[Diev]
  val disjunction = test2[\/]
  val dList = test1[DList]
  val either = test2[Either]
  val eitherT = test[EitherT[Maybe, Int, Int]]
  val ephemeralStream = test1[EphemeralStream]
//  val gen = test1[Gen]
  val heap = test1[Heap]
  val iList = test1[IList]
  val iMap = test2[IMap]
  val immutableArray = test1[ImmutableArray]
  val indSeq = test1[IndSeq]
  val iSet = test1[ISet]
  val lazyEither = test2[LazyEither]
  // LazyEitherT
  val lazyOption = test1[LazyOption]
  val lazyTuple2 = test2[LazyTuple2]
  val lazyTuple3 = test[LazyTuple3[Int, Int, Int]]
  val lazyTuple4 = test[LazyTuple4[Int, Int, Int, Int]]
  val list = test1[List]
  val map = test2[Map]
  val maybe = test1[Maybe]
  val maybeT = test[MaybeT[Maybe, Int]]
  val nonEmptyList = test1[NonEmptyList]
 // val nullArgument = test2[NullArgument]
  //val nullResult = test2[NullResult]
  val oneAnd = test[OneAnd[Maybe, Int]]
  val oneOr = test[OneOr[Maybe, Int]]
  val option = test1[Option]
  val optionT = test[OptionT[Maybe, Int]]
//  val partialFunction = test2[PartialFunction]
  val rand = test[Rand]
  val set = test1[Set]
//  val stateT = test[IndexedStateT[Maybe, Int, Int, Int]]
  val streamT = test[StreamT[Maybe, Int]]
  val string = test[String @@ AlphaNum]




}
