package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.Id.Id
import scalaz.std.AllInstances.*

object FreeTTest extends Scalaprops {
  import FreeTest.*

  implicit def freeTEqual[S[_]: Functor: Eq1, M[_]: BindRec: Applicative: Eq1, A: Equal]: Equal[FreeT[S, M, A]] =
    new Equal[FreeT[S, M, A]] {
      def equal(a: FreeT[S, M, A], b: FreeT[S, M, A]) = {
        implicit val s: Equal[S[FreeT[S, M, A]]] = Eq1[S].eq1(using freeTEqual[S, M, A])
        Eq1[M].eq1[S[FreeT[S, M, A]] \/ A].equal(a.resume, b.resume)
      }
    }

  property("isoIList") = {
    type B = Byte
    type F[A] = IList[A]
    type X = FreeT[F, Id, B]
    type Y = Free[F, B]
    scalazlaws.iso.all[X, Y](FreeT.isoFree[F].unlift[B])
  }

  property("isoMaybe") = {
    type B = Byte
    type F[A] = Maybe[A]
    type X = FreeT[F, Id, B]
    type Y = Free[F, B]
    scalazlaws.iso.all[X, Y](FreeT.isoFree[F].unlift[B])
  }

  property("isoTree") = {
    type B = Byte
    type F[A] = Tree[A]
    type X = FreeT[F, Id, B]
    type Y = Free[F, B]
    scalazlaws.iso.all[X, Y](FreeT.isoFree[F].unlift[B])
  }

  property("isoDisjunction") = {
    type B = Byte
    type F[A] = Boolean \/ A
    type X = FreeT[F, Id, B]
    type Y = Free[F, B]
    scalazlaws.iso.all[X, Y](FreeT.isoFree[F].unlift[B])
  }

  property("kleisliMaybe_Maybe") = {
    type G[A] = Kleisli[Maybe, Byte, A]
    type F[A] = FreeT[G, Maybe, A]
    Properties.list(
      scalazlaws.bindRec.laws[F],
      scalazlaws.monadPlus.all[F]
    )
  }

  property("maybe_kleisliMaybe") = {
    type G[A] = Kleisli[Maybe, Byte, A]
    type F[A] = FreeT[Maybe, G, A]
    Properties.list(
      scalazlaws.bindRec.laws[F],
      scalazlaws.monadPlus.all[F]
    )
  }

  property("f1Maybe") = {
    type G[A] = Byte => A
    type F[A] = FreeT[G, Maybe, A]
    Properties.list(
      scalazlaws.bindRec.laws[F],
      scalazlaws.monadPlus.all[F]
    )
  }

  property("maybeF1") = {
    type G[A] = Byte => A
    type F[A] = FreeT[Maybe, G, A]
    Properties.list(
      scalazlaws.bindRec.laws[F],
      scalazlaws.monad.all[F]
    )
  }.andThenParamPF { case Or.R(Or.L(ScalazLaw.applyComposition)) =>
    Param.maxSize(2) andThen Param.minSuccessful(10)
  }

  property("maybeMaybe") = {
    type F[A] = FreeT[Maybe, Maybe, A]
    Properties.list(
      scalazlaws.bindRec.laws[F],
      scalazlaws.traverse.all[F],
      scalazlaws.monadPlus.all[F]
    )
  }

  property("maybeIList") = {
    type F[A] = FreeT[Maybe, IList, A]
    Properties.list(
      scalazlaws.bindRec.laws[F],
      scalazlaws.traverse.all[F],
      scalazlaws.monadPlus.all[F]
    )
  }.andThenParamPF { case Or.R(Or.L(ScalazLaw.bindRecTailrecBindConsistency)) =>
    Param.maxSize(20) andThen Param.minSuccessful(10)
  }

  property("iListMaybe") = {
    type F[A] = FreeT[IList, Maybe, A]
    Properties.list(
      scalazlaws.bindRec.laws[F],
      scalazlaws.traverse.all[F],
      scalazlaws.monadPlus.all[F]
    )
  }.andThenParamPF { case Or.R(Or.L(ScalazLaw.bindRecTailrecBindConsistency)) =>
    Param.maxSize(20) andThen Param.minSuccessful(10)
  }

  property("iListIList") = {
    type F[A] = FreeT[IList, IList, A]
    Properties.list(
      scalazlaws.bindRec.laws[F],
      scalazlaws.traverse.all[F],
      scalazlaws.monadPlus.all[F]
    )
  }.andThenParamPF { case Or.R(Or.L(ScalazLaw.bindRecTailrecBindConsistency)) =>
    Param.maxSize(20) andThen Param.minSuccessful(10)
  }

  property("disjunctionDisjunction") = {
    type E = Byte
    type G[A] = E \/ A
    type H[A] = Boolean \/ A
    type F[A] = FreeT[H, G, A]
    Properties.list(
      scalazlaws.bindRec.laws[F],
      scalazlaws.traverse.all[F],
      scalazlaws.monadError.all[F, E]
    )
  }

  property("maybeDisjunction") = {
    type E = Byte
    type G[A] = E \/ A
    type F[A] = FreeT[Maybe, G, A]
    Properties.list(
      scalazlaws.bindRec.laws[F],
      scalazlaws.traverse.all[F],
      scalazlaws.monadError.all[F, E]
    )
  }

  property("iListDisjunction") = {
    type E = Byte
    type G[A] = E \/ A
    type F[A] = FreeT[IList, G, A]
    Properties.list(
      scalazlaws.bindRec.laws[F],
      scalazlaws.traverse.all[F],
      scalazlaws.monadError.all[F, E]
    )
  }.andThenParamPF { case Or.R(Or.L(ScalazLaw.bindRecTailrecBindConsistency)) =>
    Param.maxSize(20) andThen Param.minSuccessful(10)
  }

  property("freeMaybe_Disjunction") = {
    type E = Byte
    type G[A] = E \/ A
    type H[A] = Free[Maybe, A]
    type F[A] = FreeT[H, G, A]
    Properties.list(
      scalazlaws.bindRec.laws[F],
      scalazlaws.traverse.all[F],
      scalazlaws.monadError.all[F, E]
    )
  }

  property("freeIList_Disjunction") = {
    type E = Byte
    type G[A] = E \/ A
    type H[A] = Free[IList, A]
    type F[A] = FreeT[H, G, A]
    Properties.list(
      scalazlaws.bindRec.laws[F],
      scalazlaws.traverse.all[F],
      scalazlaws.monadError.all[F, E]
    )
  }

  property("freeDisjunction_Disjunction") = {
    type E = Byte
    type G[A] = E \/ A
    type H[A] = Free[G, A]
    type F[A] = FreeT[H, G, A]
    Properties.list(
      scalazlaws.bindRec.laws[F],
      scalazlaws.traverse.all[F],
      scalazlaws.monadError.all[F, E]
    )
  }

  property("idState") = {
    type S = Byte
    type G[A] = State[S, A]
    type F[A] = FreeT[Id, G, A]
    Properties.list(
      scalazlaws.bindRec.laws[F],
      scalazlaws.monadState.all[F, S]
    )
  }.andThenParamPF { case Or.R(Or.L(ScalazLaw.applyComposition)) =>
    Param.maxSize(2) andThen Param.minSuccessful(10)
  }

  property("idStateMaybe") = {
    type S = Byte
    type G[A] = StateT[S, Maybe, A]
    type F[A] = FreeT[Id, G, A]
    Properties.list(
      // TODO diverging implicit expansion for type scalaprops.Gen[Int => F[Int]]
      // scalazlaws.bindRec.laws[F],
      scalazlaws.monadState.all[F, S]
    )
  }.andThenParamPF { case Or.R(Or.L(ScalazLaw.applyComposition)) =>
    Param.maxSize(2) andThen Param.minSuccessful(10)
  }

  property("maybeState") = {
    type S = Byte
    type G[A] = State[S, A]
    type F[A] = FreeT[Maybe, G, A]
    Properties.list(
      scalazlaws.bindRec.laws[F],
      scalazlaws.monadState.all[F, S]
    )
  }.andThenParamPF { case Or.R(Or.L(ScalazLaw.applyComposition)) =>
    Param.maxSize(2) andThen Param.minSuccessful(10)
  }

  property("maybeStateIList") = {
    type S = Byte
    type G[A] = StateT[S, IList, A]
    type F[A] = FreeT[Maybe, G, A]
    Properties.list(
      scalazlaws.monadState.all[F, S]
    )
  }.andThenParamPF { case Or.R(Or.L(ScalazLaw.applyComposition)) =>
    Param.maxSize(2) andThen Param.minSuccessful(10)
  }

  property("iListStateIList") = {
    val laws = Set(ScalazLaw.bindAssociativity, ScalazLaw.applyComposition)

    type S = Byte
    type G[A] = StateT[S, IList, A]
    type F[A] = FreeT[IList, G, A]
    Properties
      .list(
        scalazlaws.monadState.all[F, S]
      )
      .andThenParam(Param.maxSize(3))
      .andThenParamPF {
        case Or.R(Or.L(law)) if laws(law) =>
          Param.maxSize(2).andThen(Param.minSuccessful(10))
      }
  }

  property("monadTransMaybe") = scalazlaws.monadTrans.all[({ type l[a[_], b] = FreeT[Maybe, a, b] })#l]
  property("monadTransIList") = scalazlaws.monadTrans.all[({ type l[a[_], b] = FreeT[IList, a, b] })#l]
}
