package scalaprops

import scalaz.{@@, Apply, Cofree, Equal, Tags}
import ScalapropsScalaz._

object CofreeTestInstances {

  private[scalaprops] type CofreeZip[F[_], A] = Cofree[F, A] @@ Tags.Zip

  object CofreeGenImplicit {
    implicit def cofreeGen[F[_], A](implicit
      F: Gen1[F],
      A: Gen[A]
    ): Gen[Cofree[F, A]] =
      Apply[Gen].apply2(A, F.gen1[Cofree[F, A]])((h, t) => Cofree(h, t))

    implicit def cofreeZipGen[F[_], A](implicit
      F: Gen1[F],
      A: Gen[A]
    ): Gen[CofreeZip[F, A]] =
      Tags.Zip.subst(cofreeGen[F, A])
  }

  implicit def cogenCofree[F[_], A](implicit
    A: Cogen[A],
    F: Cogen1[F]
  ): Cogen[Cofree[F, A]] =
    new Cogen[Cofree[F, A]] {
      override def cogen[B](a: Cofree[F, A], g: CogenState[B]) =
        A.cogen(a.head, F.cogen1[Cofree[F, A]].cogen(a.tail, g))
    }

  implicit def cofreeEqual[F[_], A](implicit
    F: Eq1[F],
    A: Equal[A]
  ): Equal[Cofree[F, A]] =
    Equal.equal((a, b) => A.equal(a.head, b.head) && F.eq1[Cofree[F, A]].equal(a.tail, b.tail))

  implicit def cofreeZipEqual[F[_], A](implicit
    F: Eq1[F],
    A: Equal[A]
  ): Equal[CofreeZip[F, A]] =
    Tags.Zip.subst(cofreeEqual[F, A])

}
