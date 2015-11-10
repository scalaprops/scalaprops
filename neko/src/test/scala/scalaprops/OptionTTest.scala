package scalaprops

import cats.data.OptionT
import cats.std.all._

object OptionTTest extends Scalaprops {

  implicit def optionTGen[F[_], A](implicit G: Gen[F[Option[A]]]): Gen[OptionT[F, A]] =
    Gen.from1(OptionT.apply[F, A])

  val option = neko.monad.all[({type l[a] = OptionT[Option, a]})#l]
  val list = neko.monad.all[({type l[a] = OptionT[List, a]})#l]
}
