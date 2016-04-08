package scalaprops

import scalaz._
import scalaz.std.anyVal._

object FreeTest extends Scalaprops {

  implicit def freeEqual[F[_]: Functor, A](implicit
    F: shapeless.Lazy[Equal[F[Free[F, A]]]],
    A: Equal[A]
  ): Equal[Free[F, A]] =
    Equal.equal((aa, bb) =>
      (aa.resume, bb.resume) match {
        case (-\/(a), -\/(b)) =>
          F.value.equal(a, b)
        case (\/-(a), \/-(b)) =>
          A.equal(a, b)
        case _ =>
          false
      }
    )

  private[this] implicit def freeGen[F[_]: Applicative, A](implicit
    F: Gen[F[A]],
    A: Gen[A]
  ): Gen[Free[F, A]] =
    Gen.oneOf(
      A.map(Free.point(_)),
      A.map(Free.return_(_)),
      F.map(Free.liftF(_))
    )

  val testMaybe = {
    type F[A] = Free[Maybe, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.traverse.all[F]
    )
  }

  val testIList = {
    type F[A] = Free[IList, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.traverse.all[F]
    )
  }

  val testNel = {
    type F[A] = Free[NonEmptyList, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.traverse1.all[F]
    )
  }

  val testDisjunction = {
    type S[a] = Int \/ a
    type F[A] = Free[S, A]
    implicit val m = Free.freeMonad[S]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.traverse.all[F]
    )
  }
}
