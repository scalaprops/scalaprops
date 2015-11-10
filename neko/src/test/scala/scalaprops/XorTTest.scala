package scalaprops

import cats.data.{XorT, Xor}
import cats.std.all._

object XorTTest extends Scalaprops {

  // TODO move
  implicit def xorGen[A: Gen, B: Gen]: Gen[A Xor B] =
    Gen.oneOf(
      Gen[A].map(Xor.left),
      Gen[B].map(Xor.right)
    )

  implicit def xorTGen[F[_], A, B](implicit G: Gen[F[A Xor B]]): Gen[XorT[F, A, B]] =
    Gen.from1(XorT.apply[F, A, B])

  val option = neko.monad.all[({type l[a] = XorT[Option, Byte, a]})#l]
  val list = neko.monad.all[({type l[a] = XorT[List, Byte, a]})#l]
}
