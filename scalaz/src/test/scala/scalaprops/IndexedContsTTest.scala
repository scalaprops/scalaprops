package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*

@scalajs.js.annotation.JSExportAll
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

  val maybeMaybe = bindTest[Maybe, Maybe]
  val iListIList = bindTest[IList, IList]
  val maybeIList = bindTest[Maybe, IList]
  val iListMaybe = bindTest[IList, Maybe]

  val testContsTMaybe = {
    type F[A] = ContsT[NonEmptyList, Int, Maybe, A]
    scalazlaws.monadPlus.all[F].andThenParam(Param.maxSize(4))
  }

  val contsTTreeMaybe = {
    type F[A] = ContsT[Tree, Int, Maybe, A]
    scalazlaws.monadPlus.all[F].andThenParam(Param.maxSize(4))
  }

  val testContsTIList = {
    type F[A] = ContsT[NonEmptyList, Int, IList, A]
    scalazlaws.monadPlus.all[F]
  }

  val testCont = {
    type F[A] = Conts[NonEmptyList, Int, A]
    scalazlaws.monad.all[F]
  }

  val testBifunctorMaybeMaybe =
    scalazlaws.bifunctor.all[({ type F[A, B] = IndexedContsT[Maybe, A, Int, Maybe, B] })#F]

  val testBifunctorMaybeIList =
    scalazlaws.bifunctor.all[({ type F[A, B] = IndexedContsT[Maybe, A, Int, IList, B] })#F]

  val testBifunctorIListMaybe =
    scalazlaws.bifunctor.all[({ type F[A, B] = IndexedContsT[IList, A, Int, Maybe, B] })#F]

  val testContravariantMaybeMaybe =
    scalazlaws.contravariant.all[({ type F[A] = IndexedContsT[Maybe, Int, A, Maybe, Int] })#F]

  val testContravariantMaybeIList =
    scalazlaws.contravariant.all[({ type F[A] = IndexedContsT[Maybe, Int, A, IList, Int] })#F]

  val testContravariantIListMaybe =
    scalazlaws.contravariant.all[({ type F[A] = IndexedContsT[IList, Int, A, Maybe, Int] })#F]
}
