package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._

object LazyTupleTest extends Scalaprops {

  private[this] type A = Int
  private[this] type B = Int
  private[this] type C = Int
  private[this] type D = Int

  val testLazyTuple2Bitraverse = scalazlaws.bitraverse.all[LazyTuple2]

  val testLazyTuple2 = Properties.either(
    "LazyTuple2",
    scalazlaws.associative.all[LazyTuple2],
    scalazlaws.order.all[LazyTuple2[A, B]],
    scalazlaws.monoid.all[LazyTuple2[A, B]],
    scalazlaws.monad.all[({type l[a] = LazyTuple2[A, a]})#l]
  )

  val testLazyTuple3 = Properties.either(
    "LazyTuple3",
    scalazlaws.order.all[LazyTuple3[A, B, C]],
    scalazlaws.monoid.all[LazyTuple3[A, B, C]],
    scalazlaws.monad.all[({type l[a] = LazyTuple3[A, B, a]})#l]
  )

  val testLazyTuple4 = Properties.either(
    "LazyTuple4",
    scalazlaws.order.all[LazyTuple4[A, B, C, D]],
    scalazlaws.monoid.all[LazyTuple4[A, B, C, D]],
    scalazlaws.monad.all[({type l[a] = LazyTuple4[A, B, C, a]})#l]
  )

}
