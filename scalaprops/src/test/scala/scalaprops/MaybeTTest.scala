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
    type E = Byte
    type G[A] = E \/ A
    type F[A] = MaybeT[G, A]
    scalazlaws.monadError.all[F, E]
  }

  val id = {
    type F[A] = MaybeT[Id.Id, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.bindRec.laws[F],
      scalazlaws.equal.all[F[Byte]]
    )
  }

  val bindRecIList =
    scalazlaws.bindRec.laws[({type l[a] = MaybeT[IList, a]})#l].andThenParam(Param.maxSize(1))

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
      scalazlaws.bindRec.all[F],
      scalazlaws.equal.all[F[Byte]]
    )
  }

  val monadTrans = scalazlaws.monadTrans.all[MaybeT]

}
