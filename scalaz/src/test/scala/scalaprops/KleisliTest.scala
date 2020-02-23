package scalaprops

import scalaprops.internal._
import scalaz.{Endo => _, _}
import scalaz.std.tuple._
import scalaz.std.anyVal._
import ScalapropsScalaz._

object KleisliTest extends Scalaprops {
  private[this] val e = new FunctionEqual(3)

  implicit def kleisliEqual[F[_], A: Gen, B](implicit E: Equal[F[B]]): Equal[Kleisli[F, A, B]] = {
    import e._
    Equal[A => F[B]].contramap(_.run)
  }

  private def kleisliTest[F[_]: MonadPlus: Zip](
    implicit
    F: Equal[F[Int]],
    E1: Equal[F[(Int, Int)]],
    E2: Equal[F[(Int, (Int, Int))]],
    G1: Gen[Kleisli[F, Int, Int]],
    G2: Gen[Kleisli[F, Int, Int => Int]]
  ) = {
    type K1[a] = Kleisli[F, Int, a]
    type K2[a, b] = Kleisli[F, a, b]

    Properties.list(
      scalazlaws.monadPlus.all[K1],
      scalazlaws.zip.all[K1],
      scalazlaws.arrow.all[K2]
    )
  }

  private val sizeSetting = Foldable1[NonEmptyList].foldLeft1(
    NonEmptyList(
      ScalazLaw.composeAssociative,
      ScalazLaw.plusAssociative,
      ScalazLaw.semigroupAssociative
    ).map { law => { case `law` => Param.maxSize(30) }: PartialFunction[ScalazLaw, Endo[Param]] }
  )(_ orElse _)

  val testMaybe = kleisliTest[Maybe]

  val disjunctionMonadError = {
    type E = Byte
    type G[A] = E \/ A
    type F[A] = Kleisli[G, Int, A]
    scalazlaws.monadError.laws[F, E]
  }

  val testIList = kleisliTest[IList].andThenParamPF {
    case Or.R(Or.L(p)) if sizeSetting.isDefinedAt(p) => sizeSetting(p)
  }

  val testNonEmptyList = {
    type K1[a] = Kleisli[NonEmptyList, Byte, a]
    type K2[a, b] = Kleisli[NonEmptyList, a, b]

    Properties.list(
      scalazlaws.monad.all[K1],
      scalazlaws.plus.all[K1],
      scalazlaws.zip.all[K1],
      scalazlaws.arrow.all[K2]
    )
  }.andThenParamPF {
    case Or.R(Or.L(p)) if sizeSetting.isDefinedAt(p) => sizeSetting(p)
  }

  val monadTrans = scalazlaws.monadTrans.all[({ type l[f[_], a] = Kleisli[f, Int, a] })#l]

  val kleisliId = {
    type F[A] = Kleisli[Id.Id, Byte, A]
    scalazlaws.bindRec.laws[F]
  }

  val maybeBindRec = scalazlaws.bindRec.laws[({ type l[a] = Kleisli[Maybe, Byte, a] })#l]

  val ilistBindRec = {
    scalazlaws.bindRec.laws[({ type l[a] = Kleisli[IList, Byte, a] })#l]
  }.andThenParam(Param.maxSize(1))
}
