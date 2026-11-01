package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object ListTTest extends Scalaprops {
  property("maybe") = scalazlaws.monad.all[({ type l[a] = ListT[Maybe, a] })#l]

  property("disableTestList") = scalazlaws.bind
    .laws[({ type l[a] = ListT[IList, a] })#l]
    .andThenParam(Param.maxSize(2))
    .ignore("https://github.com/scalaz/scalaz/issues/921")

  property("monadTrans") = scalazlaws.monadTrans.all[ListT]
}
