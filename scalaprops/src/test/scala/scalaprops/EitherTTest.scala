package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.tuple._

object EitherTTest extends Scalaprops {

  private[this] implicit def eitherTMonadState[F[_, _], A1](implicit F: MonadState[F, A1]): MonadState[({type x[a, b] = EitherT[({type y[c] = F[A1, c]})#y, a, b]})#x, A1] =
    new MonadState[({type x[a, b] = EitherT[({type y[c] = F[A1, c]})#y, a, b]})#x, A1] {
      type G[A] = F[A1, A]
      private[this] val M = MonadTrans[({type l[a[_], b] = EitherT[a, A1, b]})#l]

      override val init =
        M.liftMU(F.init)

      override val get =
        M.liftMU(F.get)

      override def put(s: A1) =
        M.liftMU(F.put(s))

      override def point[A](a: => A) =
        EitherT.eitherTMonad[G, A1].point(a)

      override def bind[A, B](fa: EitherT[G, A1, A])(f: A => EitherT[G, A1, B]) =
        fa flatMap f
    }

  private[this] final class EitherTMonadStateTestHelper[F[_], S0]{
    type S = S0
    type F1[A] = F[A]
    type F2[A, B] = StateT[F1, A, B]
    type F3[A] = F2[S, A]
    type F4[A, B] = EitherT[F3, A, B]
  }

  val monadStateId = {
    import FunctionEqual._
    import StateTTest.stateTEqual
    val H = new EitherTMonadStateTestHelper[Id.Id, Byte]
    import H._
    scalazlaws.monadState.laws[F4, S]
  }

  val monadStateIList = {
    import FunctionEqual._
    import StateTTest.stateTEqual
    val H = new EitherTMonadStateTestHelper[IList, Byte]
    import H._
    scalazlaws.monadState.laws[F4, S]
  }

  val monadStateMaybe = {
    import FunctionEqual._
    import StateTTest.stateTEqual
    val H = new EitherTMonadStateTestHelper[Maybe, Byte]
    import H._
    scalazlaws.monadState.laws[F4, S]
  }


  val testEitherTMaybe =
    Properties.list(
      scalazlaws.equal.all[EitherT[Maybe, Int, Int]],
      scalazlaws.monadPlus.all[({type l[a] = EitherT[Maybe, Int, a]})#l],
      scalazlaws.monadError.all[({type l[a, b] = EitherT[Maybe, a, b]})#l, Int],
      scalazlaws.traverse.all[({type l[a] = EitherT[Maybe, Int, a]})#l]
    )

  val testEitherTMaybe2 =
    scalazlaws.bitraverse.all[({type l[a, b] = EitherT[Maybe, a, b]})#l]

  val testEitherTIList =
    Properties.list(
      scalazlaws.equal.all[EitherT[IList, Int, Int]],
      scalazlaws.monadPlus.all[({type l[a] = EitherT[IList, Int, a]})#l],
      scalazlaws.monadError.all[({type l[a, b] = EitherT[IList, a, b]})#l, Int],
      scalazlaws.traverse.all[({type l[a] = EitherT[IList, Int, a]})#l]
    )

  val testEitherTNel =
    Properties.list(
      scalazlaws.equal.all[EitherT[NonEmptyList, Int, Int]],
      scalazlaws.monadPlus.all[({type l[a] = EitherT[NonEmptyList, Int, a]})#l],
      scalazlaws.monadError.all[({type l[a, b] = EitherT[NonEmptyList, a, b]})#l, Int],
      scalazlaws.traverse.all[({type l[a] = EitherT[NonEmptyList, Int, a]})#l]
    )

  val monadTrans = scalazlaws.monadTrans.all[({type l[f[_], a] = EitherT[f, Int, a]})#l]

}
