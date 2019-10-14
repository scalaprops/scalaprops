package scalaprops

import scala.util.Random
import scalaz._

object FunctionEqual extends FunctionEqual(5)

sealed class FunctionEqual(size: Int) {
  implicit def f1[A1: Gen, B](implicit B: Equal[B]): Equal[A1 => B] = {
    val values = Gen[A1].samples(listSize = size, size = size, seed = Random.nextLong())

    Equal.equal((x, y) => values.forall(a => B.equal(x(a), y(a))))
  }

  implicit def f2[A1: Gen, A2: Gen, B](implicit B: Equal[B]): Equal[(A1, A2) => B] =
    f1[(A1, A2), B].contramap(_.tupled)

  implicit def f3[A1: Gen, A2: Gen, A3: Gen, B](implicit B: Equal[B]): Equal[(A1, A2, A3) => B] =
    f1[(A1, A2, A3), B].contramap(_.tupled)

  implicit def f4[A1: Gen, A2: Gen, A3: Gen, A4: Gen, B](implicit B: Equal[B]): Equal[(A1, A2, A3, A4) => B] =
    f1[(A1, A2, A3, A4), B].contramap(_.tupled)

  implicit def f5[A1: Gen, A2: Gen, A3: Gen, A4: Gen, A5: Gen, B](
    implicit B: Equal[B]
  ): Equal[(A1, A2, A3, A4, A5) => B] =
    f1[(A1, A2, A3, A4, A5), B].contramap(_.tupled)

  implicit def f6[A1: Gen, A2: Gen, A3: Gen, A4: Gen, A5: Gen, A6: Gen, B](
    implicit B: Equal[B]
  ): Equal[(A1, A2, A3, A4, A5, A6) => B] =
    f1[(A1, A2, A3, A4, A5, A6), B].contramap(_.tupled)

  implicit def f7[A1: Gen, A2: Gen, A3: Gen, A4: Gen, A5: Gen, A6: Gen, A7: Gen, B](
    implicit B: Equal[B]
  ): Equal[(A1, A2, A3, A4, A5, A6, A7) => B] =
    f1[(A1, A2, A3, A4, A5, A6, A7), B].contramap(_.tupled)

  implicit def f8[A1: Gen, A2: Gen, A3: Gen, A4: Gen, A5: Gen, A6: Gen, A7: Gen, A8: Gen, B](
    implicit B: Equal[B]
  ): Equal[(A1, A2, A3, A4, A5, A6, A7, A8) => B] =
    f1[(A1, A2, A3, A4, A5, A6, A7, A8), B].contramap(_.tupled)
}
