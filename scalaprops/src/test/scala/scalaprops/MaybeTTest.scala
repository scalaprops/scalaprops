package scalaprops

import scalaz._
import scalaz.std.anyVal._

object MaybeTTest extends Scalaprops {

  private[this] implicit def maybeTMonadState[F[_, _], S](implicit F: MonadState[F, S]): MonadState[({type x[a, b] = MaybeT[({type y[c] = F[a, c]})#y, b]})#x, S] =
    new MonadState[({type x[a, b] = MaybeT[({type y[c] = F[a, c]})#y, b]})#x, S] {
      type G[A] = F[S, A]
      private[this] val M = MonadTrans[({type l[a[_], b] = MaybeT[a, b]})#l]

      override def init =
        M.liftMU(F.init)

      override def get =
        M.liftMU(F.get)

      override def put(s: S) =
        M.liftMU(F.put(s))

      override def bind[A, B](fa: MaybeT[G, A])(f: A => MaybeT[G, B]) =
        fa flatMap f

      override def point[A](a: => A) =
        MaybeT.maybeTMonadPlus[G].point(a)
    }

  val disjunction = {
    type F[E, A] = MaybeT[({type l[a] = E \/ a})#l, A]

    implicit def gen[G[_, _], E, A](implicit G: Gen[G[E, Maybe[A]]]) =
      Gen.maybeTGen[({type l[a] = G[E, a]})#l, A]

    implicit def equal[G[_, _], E, A](implicit G: Equal[G[E, Maybe[A]]]) =
      MaybeT.maybeTEqual[({type l[a] = G[E, a]})#l, A]

    scalazlaws.monadError.all[F, Byte]
  }

  val testLawsIList = {
    type F[A] = MaybeT[IList, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Byte]]
    )
  }

  val testLawsMaybe = {
    type F[A] = MaybeT[Maybe, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Byte]]
    )
  }

  val monadTrans = scalazlaws.monadTrans.all[MaybeT]

}
