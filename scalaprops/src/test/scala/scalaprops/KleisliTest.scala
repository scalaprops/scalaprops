package scalaprops

import scalaz._
import scalaz.std.string._
import scalaz.std.tuple._
import scalaz.std.anyVal._

object KleisliTest extends Scalaprops {

  private[this] val e = new FunctionEqual(3)

  implicit def kleisliEqual[F[_], A: Gen, B](implicit E: Equal[F[B]]): Equal[Kleisli[F, A, B]] = {
    import e._
    Equal[A => F[B]].contramap(_.run)
  }

  private def kleisliTest[F[_]: Monad](implicit
    F: Equal[F[Int]],
    E1: Equal[F[(Int, Int)]],
    E2: Equal[F[(Int, (Int, Int))]],
    E3: Equal[F[((Int, Int), Int)]],
    G1: Gen[Kleisli[F, Int, Int]],
    G2: Gen[Kleisli[F, Int, Int => Int]]
  ) = {
    type K1[a] = Kleisli[F, Int, a]
    type K2[a, b] = Kleisli[F, a, b]

    Properties.either(
      "Kleisli",
      scalazlaws.monad.all[K1],
      scalazlaws.arrow.all[K2]
    )
  }

  private val sizeSetting = Foldable1[NonEmptyList].foldLeft1(
    NonEmptyList(
      ScalazLaw.bindApConsistentWithBind,
      ScalazLaw.composeAssociative,
      ScalazLaw.plusAssociative,
      ScalazLaw.semigroupAssociative
    ).map { law =>
      { case `law` => Param.maxSize(30) }: PartialFunction[ScalazLaw, Endo[Param]]
    }
  )(_ orElse _)


  val testMaybe = kleisliTest[Maybe]

  val testIList = kleisliTest[IList].andThenParamPF{
    case \/-(p) if sizeSetting.isDefinedAt(p) => sizeSetting(p)
  }

  val testNonEmptyList = kleisliTest[NonEmptyList].andThenParamPF{
    case \/-(p) if sizeSetting.isDefinedAt(p) => sizeSetting(p)
  }

}
