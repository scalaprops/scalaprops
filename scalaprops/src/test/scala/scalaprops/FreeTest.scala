package scalaprops

import scalaz._
import scalaz.std.anyVal._

object FreeTest extends Scalaprops {

  implicit def freeEqual[F[_]: Functor, A](implicit
    E: Eq1[F],
    A: Equal[A]
  ): Equal[Free[F, A]] =
    Equal.equal((aa, bb) =>
      (aa.resume, bb.resume) match {
        case (-\/(a), -\/(b)) =>
          E.eq1[Free[F, A]].equal(a, b)
        case (\/-(a), \/-(b)) =>
          A.equal(a, b)
        case _ =>
          false
      }
    )

  implicit def freeGen[F[_]: Applicative, A](implicit
    F: Gen[F[A]],
    A: Gen[A]
  ): Gen[Free[F, A]] =
    Gen.oneOf(
      A.map(Free.point(_)),
      A.map(Free.return_(_)),
      F.map(Free.liftF(_))
    )


  val bindRecIList =
    scalazlaws.bindRec.laws[({type l[a] = Free[IList, a]})#l].andThenParam(Param.maxSize(1))

  val testMaybe = {
    type F[A] = Free[Maybe, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.zip.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.traverse.all[F]
    )
  }

  val testIList = {
    type F[A] = Free[IList, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.zip.all[F],
      scalazlaws.traverse.all[F]
    )
  }

  val testNel = {
    type F[A] = Free[NonEmptyList, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.zip.all[F],
      scalazlaws.traverse1.all[F]
    )
  }

  val testDisjunction = {
    type S[a] = Int \/ a
    type F[A] = Free[S, A]
    implicit val m = Free.freeMonad[S]
    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.bindRec.all[F],
      scalazlaws.traverse.all[F]
    )
  }

  val sourceUnit = {
    import Free.Source
    import scalaz.std.AllInstances._

    implicit object UnitSourceInstance extends MonadPlus[Source[?, Unit]] {
      private type F[A] = Source[A, Unit]
      override def bind[A, B](fa: F[A])(f: (A) => F[B]): F[B] = {
        fa.resume match {
          case -\/((head, tailSource)) => {
            f(head).flatMap { _: Unit =>
              bind(tailSource)(f)
            }
          }
          case \/-(a) =>
            Free.point[(B, ?), Unit](())
        }
      }
      override def empty[A]: F[A] = Free.point[(A, ?), Unit](())
      override def plus[A](a: F[A], b: => F[A]): F[A] =  a.flatMap { _: Unit => b }
      override def point[A](a: => A): F[A] = Free.produce(a)
    }

    scalazlaws.monadPlusStrong.all[({type l[a] = Free.Source[a, Unit]})#l]
  }
}
