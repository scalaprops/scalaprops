package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object LazyTupleTest extends Scalaprops {
  private[this] type A = Int
  private[this] type B = Int
  private[this] type C = Int
  private[this] type D = Int

  property("lazyTuple2Bitraverse") = scalazlaws.bitraverse.all[LazyTuple2]

  property("lazyTuple2") = Properties.list(
    scalazlaws.associative.all[LazyTuple2],
    scalazlaws.order.all[LazyTuple2[A, B]],
    scalazlaws.monoid.all[LazyTuple2[A, B]],
    scalazlaws.monad.all[({ type l[a] = LazyTuple2[A, a] })#l]
  )

  property("lazyTuple3") = Properties.list(
    scalazlaws.order.all[LazyTuple3[A, B, C]],
    scalazlaws.monoid.all[LazyTuple3[A, B, C]],
    scalazlaws.monad.all[({ type l[a] = LazyTuple3[A, B, a] })#l]
  )

  property("lazyTuple4") = Properties.list(
    scalazlaws.order.all[LazyTuple4[A, B, C, D]],
    scalazlaws.monoid.all[LazyTuple4[A, B, C, D]],
    scalazlaws.monad.all[({ type l[a] = LazyTuple4[A, B, C, a] })#l]
  )
}
