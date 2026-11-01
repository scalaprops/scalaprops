package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

object IndexedContsTTest extends Scalaprops {
  private[this] val F = new FunctionEqual(5)
  import F.*

  private[this] implicit def indexedContsTEqual[W[_], M[_], R, O, A](implicit
    F: Equal[W[A => M[O]] => M[R]]
  ): Equal[IndexedContsT[W, R, O, M, A]] =
    F.contramap(_.run)

  private[this] def bindTest[W[_]: Cobind, M[_]](implicit
    G1: Gen[ContsT[W, Int, M, Int]],
    G2: Gen[ContsT[W, Int, M, Int => Int]],
    E1: Equal[ContsT[W, Int, M, Int]]
  ) = {
    type F[A] = ContsT[W, Int, M, A]
    scalazlaws.bind.all[F].andThenParam(Param.maxSize(3))
  }

  property("maybeMaybe") = bindTest[Maybe, Maybe]
  property("iListIList") = bindTest[IList, IList]
  property("maybeIList") = bindTest[Maybe, IList]
  property("iListMaybe") = bindTest[IList, Maybe]

  property("contsTMaybe") = {
    type F[A] = ContsT[NonEmptyList, Int, Maybe, A]
    scalazlaws.monadPlus.all[F].andThenParam(Param.maxSize(4))
  }

  property("contsTTreeMaybe") = {
    type F[A] = ContsT[Tree, Int, Maybe, A]
    scalazlaws.monadPlus.all[F].andThenParam(Param.maxSize(4))
  }

  property("contsTIList") = {
    type F[A] = ContsT[NonEmptyList, Int, IList, A]
    scalazlaws.monadPlus.all[F]
  }

  property("cont") = {
    type F[A] = Conts[NonEmptyList, Int, A]
    scalazlaws.monad.all[F]
  }

  property("bifunctorMaybeMaybe") =
    scalazlaws.bifunctor.all[({ type F[A, B] = IndexedContsT[Maybe, A, Int, Maybe, B] })#F]

  property("bifunctorMaybeIList") =
    scalazlaws.bifunctor.all[({ type F[A, B] = IndexedContsT[Maybe, A, Int, IList, B] })#F]

  property("bifunctorIListMaybe") =
    scalazlaws.bifunctor.all[({ type F[A, B] = IndexedContsT[IList, A, Int, Maybe, B] })#F]

  property("contravariantMaybeMaybe") =
    scalazlaws.contravariant.all[({ type F[A] = IndexedContsT[Maybe, Int, A, Maybe, Int] })#F]

  property("contravariantMaybeIList") =
    scalazlaws.contravariant.all[({ type F[A] = IndexedContsT[Maybe, Int, A, IList, Int] })#F]

  property("contravariantIListMaybe") =
    scalazlaws.contravariant.all[({ type F[A] = IndexedContsT[IList, Int, A, Maybe, Int] })#F]
}
