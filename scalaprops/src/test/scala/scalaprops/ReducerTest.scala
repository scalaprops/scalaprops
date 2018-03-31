package scalaprops

import scalaz._
import scalaz.Tags._
import scalaz.std.AllInstances._
import EndoTest.endoEqual

object ReducerTest extends Scalaprops{

  private[this] implicit def tagGen[A: Gen, B]: Gen[A @@ B] =
    Tag.subst(Gen[A])

  private[this] implicit def tagEqual[A: Equal, B]: Equal[A @@ B] =
    Tag.subst(Equal[A])

  val list = scalazlaws.reducer.all[Int, List[Int]]
  val ilist = scalazlaws.reducer.all[Int, IList[Int]]
  val stream = scalazlaws.reducer.all[Int, Stream[Int]]
  val vector = scalazlaws.reducer.all[Int, Vector[Int]]
  val boolean = scalazlaws.reducer.all[Boolean, Boolean]
  val booleanConjunction = scalazlaws.reducer.all[Boolean, Boolean @@ Conjunction]
  val endo = scalazlaws.reducer.all[Int => Int, Endo[Int]]
  val dual = scalazlaws.reducer.all[Int, Int @@ Dual]
  val intMultiplication = scalazlaws.reducer.all[Int, Int @@ Multiplication]
  val bigIntMultiplication = scalazlaws.reducer.all[BigInt, BigInt @@ Multiplication]
  val optionFirst = scalazlaws.reducer.all[Int, Option[Int] @@ First]
  val optionOptionFirst = scalazlaws.reducer.all[Option[Int], Option[Int] @@ First]
  val optionLast = scalazlaws.reducer.all[Int, Option[Int] @@ Last]
  val optionOptionLast = scalazlaws.reducer.all[Option[Int], Option[Int] @@ Last]

}
