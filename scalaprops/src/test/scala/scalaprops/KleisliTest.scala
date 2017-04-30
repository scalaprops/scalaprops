package scalaprops

import scalaz._
import scalaz.std.tuple._
import scalaz.std.anyVal._

object KleisliTest extends Scalaprops {

  private[this] val e = new FunctionEqual(3)

  implicit def kleisliEqual[F[_], A: Gen, B](implicit E: Equal[F[B]]): Equal[Kleisli[F, A, B]] = {
    import e._
    Equal[A => F[B]].contramap(_.run)
  }

  private def kleisliTest[F[_]: MonadPlus: Zip](implicit
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
    ).map { law =>
      { case `law` => Param.maxSize(30) }: PartialFunction[ScalazLaw, Endo[Param]]
    }
  )(_ orElse _)


  val testMaybe = kleisliTest[Maybe]

  val disjunctionMonadError = {
    implicit def gen0[F[_, _], A, B, C](implicit G: Gen[A => F[B, C]], B: Bind[({type l[a] = F[B, a]})#l]): Gen[Kleisli[({type l[a] = F[B, a]})#l, A, C]] =
      G.map(Kleisli.kleisliU(_))

    implicit def equal0[F[_, _], A, B, C](implicit E: Equal[A => F[B, C]]): Equal[Kleisli[({type l[a] = F[B, a]})#l, A, C]] =
      E.contramap(_.run)

    import e._

    scalazlaws.monadError.laws[({type x[a, b] = Kleisli[({type y[c] = a \/ c})#y, Byte, b]})#x, Byte]
  }

  val testIList = kleisliTest[IList].andThenParamPF{
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
  }.andThenParamPF{
    case Or.R(Or.L(p)) if sizeSetting.isDefinedAt(p) => sizeSetting(p)
  }

  val monadTrans = scalazlaws.monadTrans.all[({type l[f[_], a] = Kleisli[f, Int, a]})#l]

}
