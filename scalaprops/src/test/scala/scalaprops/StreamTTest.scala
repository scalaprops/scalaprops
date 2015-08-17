package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.stream._

object StreamTTest extends Scalaprops {

  val testId = {
    import scalaz.Id._
    type F[A] = StreamT[Id, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  val testOneOrTwo = {
    type OneOrTwo[A] = OneAnd[Maybe, A]
    type F[A] = StreamT[OneOrTwo, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }.andThenParam(Param.maxSize(2))

  val testMaybe = {
    type F[A] = StreamT[Maybe, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  val iList = {
    type F[A] = StreamT[IList, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }.andThenParam(Param.maxSize(3))

  val monadTrans = scalazlaws.monadTrans.all[StreamT]

}
