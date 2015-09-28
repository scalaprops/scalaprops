package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.tuple._

object WriterTTest extends Scalaprops {

  private[this] implicit def writerTMonadState[F[_, _], A1](implicit F: MonadState[F, A1], A1: Monoid[A1]): MonadState[({type x[a, b] = WriterT[({type y[c] = F[A1, c]})#y, a, b]})#x, A1] =
    new MonadState[({type x[a, b] = WriterT[({type y[c] = F[A1, c]})#y, a, b]})#x, A1] {
      type G[A] = F[A1, A]
      private[this] val M = MonadTrans[({type l[a[_], b] = WriterT[a, A1, b]})#l]

      override def init =
        M.liftMU(F.init)

      override def get =
        M.liftMU(F.get)

      override def put(s: A1) =
        M.liftMU(F.put(s))

      override def bind[A, B](fa: WriterT[G, A1, A])(f: A => WriterT[G, A1, B]) =
        WriterT.writerTMonad[G, A1].bind(fa)(f)

      override def point[A](a: => A) =
        WriterT.writerTApplicative[G, A1].point(a)
    }

  private[this] final class WriterTMonadStateTestHelper[F[_], S0]{
    type S = S0
    type F1[A] = F[A]
    type F2[A, B] = StateT[F1, A, B]
    type F3[A] = F2[S, A]
    type F4[A, B] = WriterT[F3, A, B]
  }

  val monadStateId = {
    import FunctionEqual._
    import StateTTest.stateTEqual
    val H = new WriterTMonadStateTestHelper[Id.Id, Byte]
    import H._
    scalazlaws.monadState.laws[F4, S]
  }

  val monadStateIList = {
    import FunctionEqual._
    import StateTTest.stateTEqual
    val H = new WriterTMonadStateTestHelper[IList, Byte]
    import H._
    scalazlaws.monadState.laws[F4, S]
  }

  val monadStateIMaybe = {
    import FunctionEqual._
    import StateTTest.stateTEqual
    val H = new WriterTMonadStateTestHelper[Maybe, Byte]
    import H._
    scalazlaws.monadState.laws[F4, S]
  }

  val monadStateDisjunction = {
    import FunctionEqual._
    import StateTTest.stateTEqual
    val H = new WriterTMonadStateTestHelper[({type l[a] = Short \/ a})#l, Byte]
    import H._
    scalazlaws.monadState.laws[F4, S]
  }


  val testMaybe1 = {
    type F[A] = WriterT[Maybe, Int, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  val testMaybe2 = {
    type F[A, B] = WriterT[Maybe, A, B]

    scalazlaws.bitraverse.all[F]
  }

  val iList1 = {
    type F[A] = WriterT[IList, Int, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  val tree = {
    type F[A] = WriterT[Tree, Byte, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  val either = {
    type F[A] = Byte \/ A
    type G[A] = WriterT[F, Short, A]

    implicit def writerTGen0[H[_, _], E, W, A](implicit F: Gen[H[E, (W, A)]]) =
      Gen.writerTGen[({type l[a] = H[E, a]})#l, W, A]

    implicit def writerTEqual0[H[_, _], E, W, A](implicit F: Equal[H[E, (W, A)]]) =
      WriterT.writerTEqual[({type l[a] = H[E, a]})#l, W, A]

    Properties.list(
      scalazlaws.monadError.all[({type x[e, a] = WriterT[({type y[b] = e \/ b})#y, Short, a]})#x, Byte],
      scalazlaws.traverse.all[G],
      scalazlaws.equal.all[G[Int]]
    )
  }

  val id = {
    type F[A] = Writer[Int, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.comonad.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  val monadTrans = scalazlaws.monadTrans.all[({type l[f[_], a] = WriterT[f, Int, a]})#l]

}
