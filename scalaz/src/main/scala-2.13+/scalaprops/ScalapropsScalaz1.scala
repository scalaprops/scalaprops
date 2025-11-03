package scalaprops

import scalaz.@@
import scalaz.Cofree
import scalaz.Tag
import scalaz.Tags

abstract class ScalapropsScalaz1 {
  implicit def cofreeGen[F[_], A](implicit a: Gen[A], f: => Gen[F[Cofree[F, A]]]): Gen[Cofree[F, A]] =
    a.flatMap { head =>
      f.map { tail =>
        Cofree(head, tail)
      }
    }

  implicit def cofreeZipGen[F[_], A](implicit a: Gen[A], f: => Gen[F[Cofree[F, A]]]): Gen[Cofree[F, A] @@ Tags.Zip] =
    Tag.subst(cofreeGen[F, A](a, f))

  implicit def cogenCofree[F[_], A](implicit
    a: Cogen[A],
    f: => Cogen[F[Cofree[F, A]]]
  ): Cogen[Cofree[F, A]] =
    new Cogen[Cofree[F, A]] {
      override def cogen[B](x: Cofree[F, A], g: CogenState[B]) =
        a.cogen(x.head, f.cogen(x.tail, g))
    }

  implicit def cogenCofreeZip[F[_], A](implicit
    a: Cogen[A],
    f: => Cogen[F[Cofree[F, A]]]
  ): Cogen[Cofree[F, A] @@ Tags.Zip] =
    Tag.subst(cogenCofree[F, A](a, f))

}
