package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.list._

object ListTTest extends Scalaprops {

  val testMaybe =
    scalazlaws.monad.all[({type l[a] = ListT[Maybe, a]})#l]

  type F[a] = ListT[List, a]

  val testDisableTestList =
    scalazlaws.bind.laws[({type l[a] = ListT[IList, a]})#l].andThenParam(Param.maxSize(2))
      .ignore("https://github.com/scalaz/scalaz/issues/921")

  val monadTrans = scalazlaws.monadTrans.all[ListT]

  implicit def listListShrink[A: Shrink]: Shrink[F[A]] =
    Shrink[List[List[A]]].xmap(ListT(_), _.run)

  val showingAndShrinkingFunction = {
    type A = Byte
    scalazlaws.bindS.associativity[F, A, A, A].toProperties(
      "scalaz issue 921",
      Foldable[IList].fold(
        IList(Param.maxSize(3), Param.minSuccessful(1000))
      )
    )
  }

}
