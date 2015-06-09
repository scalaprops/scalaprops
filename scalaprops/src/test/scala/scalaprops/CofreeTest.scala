package scalaprops

import scalaz._
import scalaz.std.anyVal._

object CofreeTest extends Scalaprops {

  private[this] implicit def cogenCofree[F[_], A](implicit
    A: Cogen[A],
    F: shapeless.Lazy[Cogen[F[Cofree[F, A]]]]
  ): Cogen[Cofree[F, A]] =
    new Cogen[Cofree[F, A]] {
      override def cogen[B](a: Cofree[F, A], g: CogenState[B]) =
        A.cogen(a.head, F.value.cogen(a.tail, g))
    }

  private[this] implicit def cofreeEqual[F[_], A](implicit
    F: shapeless.Lazy[Equal[F[Cofree[F, A]]]],
    A: Equal[A]
  ): Equal[Cofree[F, A]] =
    Equal.equal((a, b) =>
      A.equal(a.head, b.head) && F.value.equal(a.tail, b.tail)
    )

  val testMaybe = {
    implicit def genCofreeMaybe[A: Gen] =
      Gen[OneAnd[List, A]].map{ list =>
        Cofree.unfold(list){
          case OneAnd(a, h :: t) =>
            (a, Maybe.just(OneAnd(h, t)))
          case OneAnd(a, Nil) =>
            (a, Maybe.empty[OneAnd[List, A]])
        }
      }

    type F[A] = Cofree[Maybe, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.comonad.all[F],
      scalazlaws.traverse1.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

}
