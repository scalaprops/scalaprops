package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.string._
import scalaz.std.tuple._

object WriterTTest extends Scalaprops {

  val testMaybe1 = {
    type F[A] = WriterT[Maybe, Int, A]

    Properties.either(
      "WriterT[Maybe, Int, _]",
      scalazlaws.monad.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  val testMaybe2 = {
    type F[A, B] = WriterT[Maybe, A, B]

    Properties.either(
      "WriterT[Maybe, _, _]",
      scalazlaws.bitraverse.all[F]
    )
  }

  val iList1 = {
    type F[A] = WriterT[IList, Int, A]

    Properties.either(
      "WriterT[IList, Int, _]",
      scalazlaws.monad.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  val id = {
    type F[A] = Writer[Int, A]

    Properties.either(
      "Writer[Int, _]",
      scalazlaws.monad.all[F],
      scalazlaws.comonad.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

}
