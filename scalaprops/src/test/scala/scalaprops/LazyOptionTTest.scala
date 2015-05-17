package scalaprops

import scalaz._
import scalaz.std.anyVal._

object LazyOptionTTest extends Scalaprops{

  val iList = Properties.list(
    scalazlaws.equal.all[LazyOptionT[IList, Int]],
    scalazlaws.monad.all[({type l[a] = LazyOptionT[IList, a]})#l]
  )

  val maybe = Properties.list(
    scalazlaws.equal.all[LazyOptionT[Maybe, Int]],
    scalazlaws.monad.all[({type l[a] = LazyOptionT[Maybe, a]})#l]
  )

}
