package scalaprops

import scala.util.Random
import scalaz._

object FunctionEqual extends FunctionEqual(20)

sealed class FunctionEqual(size: Int) {
  implicit def f1[A1: Gen, B](implicit B: Equal[B]): Equal[A1 => B] = {
    val values = Gen[A1].samples(listSize = size, size = size, seed = Random.nextLong())

    Equal.equal((x, y) =>
      values.forall(a => B.equal(x(a), y(a)))
    )
  }

  implicit def f2[A1: Gen, A2: Gen, B](implicit B: Equal[B]): Equal[(A1, A2) => B] =
    f1[(A1, A2), B].contramap(_.tupled)

  implicit def f3[A1: Gen, A2: Gen, A3: Gen, B](implicit B: Equal[B]): Equal[(A1, A2, A3) => B] =
    f1[(A1, A2, A3), B].contramap(_.tupled)

  implicit def f4[A1: Gen, A2: Gen, A3: Gen, A4: Gen, B](implicit B: Equal[B]): Equal[(A1, A2, A3, A4) => B] =
    f1[(A1, A2, A3, A4), B].contramap(_.tupled)

  implicit def nt[M[_[_], _], F[_], H[_]](implicit
    G: Gen1[({type l[a] = M[F, a]})#l],
    E: Eq1[({type l[a] = M[H, a]})#l]
  ): Equal[({type l[a] = M[F, a]})#l ~> ({type l[a] = M[H, a]})#l] = {
    import scalaz.std.anyVal._
    val values = G.gen1[Byte].samples(listSize = size, seed = System.currentTimeMillis())
    val e1 = E.eq1[Byte]

    Equal.equal((x, y) =>
      values.forall(a => e1.equal(x(a), y(a)))
    )
  }
}
