package scalaprops

import EndoTest.endoEqual
import ScalapropsScalaz.*
import scalaz.*
import scalaz.Tags.*
import scalaz.std.AllInstances.*

object ReducerTest extends Scalaprops {
  private[this] implicit def tagGen[A: Gen, B]: Gen[A @@ B] =
    Tag.subst(Gen[A])

  private[this] implicit def tagEqual[A: Equal, B]: Equal[A @@ B] =
    Tag.subst(Equal[A])

  property("list") = scalazlaws.reducer.all[Int, List[Int]]
  property("ilist") = scalazlaws.reducer.all[Int, IList[Int]]
  property("nel") = scalazlaws.reducer.all[Int, NonEmptyList[Int]]
  property("stream") = scalazlaws.reducer.all[Int, Stream[Int]]
  property("vector") = scalazlaws.reducer.all[Int, Vector[Int]]
  property("boolean") = scalazlaws.reducer.all[Boolean, Boolean]
  property("booleanConjunction") = scalazlaws.reducer.all[Boolean, Boolean @@ Conjunction]
  property("endo") = scalazlaws.reducer.all[Int => Int, Endo[Int]]
  property("dual") = scalazlaws.reducer.all[Int, Int @@ Dual]
  property("intMultiplication") = scalazlaws.reducer.all[Int, Int @@ Multiplication]
  property("bigIntMultiplication") = scalazlaws.reducer.all[BigInt, BigInt @@ Multiplication]
  property("optionFirst") = scalazlaws.reducer.all[Int, Option[Int] @@ First]
  property("optionOptionFirst") = scalazlaws.reducer.all[Option[Int], Option[Int] @@ First]
  property("optionLast") = scalazlaws.reducer.all[Int, Option[Int] @@ Last]
  property("optionOptionLast") = scalazlaws.reducer.all[Option[Int], Option[Int] @@ Last]
}
